package ming;

import static ming.EvaluatorSupport.*;
import static ming.RuntimeConstants.*;
import static ming.ValueSupport.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;

final class EvaluatorBuiltins {
    @FunctionalInterface
    interface ProcedureInvoker {
        Value apply(Value operator, List<Value> arguments) throws EvalError;
    }

    @FunctionalInterface
    interface OutputEmitter {
        void append(String value);
    }

    @FunctionalInterface
    interface QuoteConverter {
        Value convert(Expr expr);
    }

    private final boolean immutableStringsEnabled;
    private final ProcedureInvoker procedureInvoker;
    private final OutputEmitter outputEmitter;
    private final QuoteConverter quoteConverter;

    private EvaluatorBuiltins(
            boolean immutableStringsEnabled,
            ProcedureInvoker procedureInvoker,
            OutputEmitter outputEmitter,
            QuoteConverter quoteConverter) {
        this.immutableStringsEnabled = immutableStringsEnabled;
        this.procedureInvoker = procedureInvoker;
        this.outputEmitter = outputEmitter;
        this.quoteConverter = quoteConverter;
    }

    static Env createGlobalEnv(
            boolean immutableStringsEnabled,
            ProcedureInvoker procedureInvoker,
            OutputEmitter outputEmitter,
            QuoteConverter quoteConverter) {
        Env env = new Env(null);
        new EvaluatorBuiltins(
                immutableStringsEnabled, procedureInvoker, outputEmitter, quoteConverter)
                .installInto(env);
        return env;
    }

    private void installInto(Env env) {
        installArithmeticBuiltins(env);
        installComparisonBuiltins(env);
        installPairAndListBuiltins(env);
        installVectorBuiltins(env);
        installProcedureBuiltins(env);
        installTypePredicates(env);
        installEqualityBuiltins(env);
        installOutputBuiltins(env);
        installNumericConversionBuiltins(env);
        installStringBuiltins(env);
        installCharacterBuiltins(env);
        installIntegerMathBuiltins(env);
    }

    private void installArithmeticBuiltins(Env env) {
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
    }

    private void installComparisonBuiltins(Env env) {
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
    }

    private void installPairAndListBuiltins(Env env) {
        env.define("cons", new BuiltinValue("cons", this::cons));
        env.define("car", new BuiltinValue("car", this::car));
        env.define("cdr", new BuiltinValue("cdr", this::cdr));
        env.define("set-car!", new BuiltinValue("set-car!", this::setCar));
        env.define("set-cdr!", new BuiltinValue("set-cdr!", this::setCdr));
        definePairAccessor(env, "caar", "aa");
        definePairAccessor(env, "cadr", "ad");
        definePairAccessor(env, "cdar", "da");
        definePairAccessor(env, "cddr", "dd");
        env.define("null?", new BuiltinValue("null?", arguments ->
                PredicateBuiltins.typePredicate("null?", arguments,
                        value -> value instanceof EmptyListValue)));
        env.define("list", new BuiltinValue("list", CollectionBuiltins::list));
        env.define("length", new BuiltinValue("length", this::length));
        env.define("list-ref", new BuiltinValue("list-ref", this::listRef));
        env.define("list-tail", new BuiltinValue("list-tail", this::listTail));
        env.define("list?", new BuiltinValue("list?", this::listPredicate));
        env.define("memq", new BuiltinValue("memq", this::memq));
        env.define("memv", new BuiltinValue("memv", this::memv));
        env.define("member", new BuiltinValue("member", this::member));
        env.define("assq", new BuiltinValue("assq", this::assq));
        env.define("assv", new BuiltinValue("assv", this::assv));
        env.define("assoc", new BuiltinValue("assoc", this::assoc));
        env.define("append", new BuiltinValue("append", this::append));
        env.define("reverse", new BuiltinValue("reverse", this::reverse));
    }

    private void installVectorBuiltins(Env env) {
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
    }

    private void installProcedureBuiltins(Env env) {
        env.define("map", new BuiltinValue("map", this::map));
        env.define("for-each", new BuiltinValue("for-each", this::forEach));
        env.define("apply", new BuiltinValue("apply", this::apply));
        env.define("call/cc", new BuiltinValue("call/cc", this::specialBuiltin));
        env.define("call-with-current-continuation",
                new BuiltinValue("call-with-current-continuation", this::specialBuiltin));
        env.define("dynamic-wind", new BuiltinValue("dynamic-wind", this::specialBuiltin));
    }

    private Value specialBuiltin(List<Value> arguments) throws EvalError {
        throw new EvalError("special builtin must be handled by the evaluator");
    }

    private void installTypePredicates(Env env) {
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
        env.define("char?", new BuiltinValue("char?", arguments ->
                PredicateBuiltins.typePredicate("char?", arguments,
                        value -> value instanceof CharValue)));
    }

    private void installEqualityBuiltins(Env env) {
        env.define("eq?", new BuiltinValue("eq?", PredicateBuiltins::eq));
        env.define("eqv?", new BuiltinValue("eqv?", PredicateBuiltins::eqv));
        env.define("equal?", new BuiltinValue("equal?", PredicateBuiltins::equal));
    }

    private void installOutputBuiltins(Env env) {
        env.define("display", new BuiltinValue("display", this::display));
        env.define("write", new BuiltinValue("write", this::write));
        env.define("newline", new BuiltinValue("newline", this::newline));
    }

    private void installNumericConversionBuiltins(Env env) {
        env.define("exact->inexact",
                new BuiltinValue("exact->inexact", PredicateBuiltins::exactToInexact));
        env.define("inexact->exact",
                new BuiltinValue("inexact->exact", PredicateBuiltins::inexactToExact));
        env.define("numerator", new BuiltinValue("numerator", PredicateBuiltins::numerator));
        env.define("denominator", new BuiltinValue("denominator", PredicateBuiltins::denominator));
    }

    private void installStringBuiltins(Env env) {
        env.define("string-append", new BuiltinValue("string-append", this::stringAppend));
        env.define("string", new BuiltinValue("string", this::string));
        env.define("make-string", new BuiltinValue("make-string", this::makeString));
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
        env.define("string>?", new BuiltinValue("string>?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string>?")));
        env.define("string<=?", new BuiltinValue("string<=?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string<=?")));
        env.define("string>=?", new BuiltinValue("string>=?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string>=?")));
        env.define("string-ci=?", new BuiltinValue("string-ci=?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string-ci=?")));
        env.define("string-upcase", new BuiltinValue("string-upcase", this::stringUpcase));
        env.define("string-downcase", new BuiltinValue("string-downcase", this::stringDowncase));
    }

    private void installCharacterBuiltins(Env env) {
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
    }

    private void installIntegerMathBuiltins(Env env) {
        env.define("gcd", new BuiltinValue("gcd", this::gcd));
        env.define("lcm", new BuiltinValue("lcm", this::lcm));
        env.define("truncate", new BuiltinValue("truncate", this::truncate));
        env.define("round", new BuiltinValue("round", this::round));
    }

    private void definePairAccessor(Env env, String name, String path) {
        env.define(name, new BuiltinValue(name, arguments -> accessPairPath(arguments, name, path)));
    }

    private Value display(List<Value> arguments) throws EvalError {
        requireExactArgs("display", arguments, 1);
        outputEmitter.append(ValueRenderer.renderDisplay(arguments.get(0)));
        return VOID;
    }

    private Value write(List<Value> arguments) throws EvalError {
        requireExactArgs("write", arguments, 1);
        outputEmitter.append(ValueRenderer.render(arguments.get(0)));
        return VOID;
    }

    private Value newline(List<Value> arguments) throws EvalError {
        requireExactArgs("newline", arguments, 0);
        outputEmitter.append("\n");
        return VOID;
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
            results.add(procedureInvoker.apply(procedure, callArguments));
        }
        return listValue(results);
    }

    private Value forEach(List<Value> arguments) throws EvalError {
        requireMinArgs("for-each", arguments, 2);

        Value procedure = arguments.get(0);
        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int expectedLength = -1;
        for (int index = 1; index < arguments.size(); index++) {
            List<Value> list = requireProperList(arguments.get(index), "for-each");
            if (expectedLength == -1) {
                expectedLength = list.size();
            } else if (list.size() != expectedLength) {
                throw new EvalError("for-each expects lists of equal length");
            }
            lists.add(list);
        }

        for (int item = 0; item < expectedLength; item++) {
            List<Value> callArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                callArguments.add(list.get(item));
            }
            procedureInvoker.apply(procedure, callArguments);
        }
        return VOID;
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

    private Value setCar(List<Value> arguments) throws EvalError {
        requireExactArgs("set-car!", arguments, 2);
        requirePair(arguments.get(0), "set-car!").setCar(arguments.get(1));
        return VOID;
    }

    private Value setCdr(List<Value> arguments) throws EvalError {
        requireExactArgs("set-cdr!", arguments, 2);
        requirePair(arguments.get(0), "set-cdr!").setCdr(arguments.get(1));
        return VOID;
    }

    private Value accessPairPath(List<Value> arguments, String name, String path) throws EvalError {
        requireExactArgs(name, arguments, 1);
        Value current = arguments.get(0);
        for (int index = path.length() - 1; index >= 0; index--) {
            PairValue pair = requirePair(current, name);
            current = switch (path.charAt(index)) {
                case 'a' -> pair.car();
                case 'd' -> pair.cdr();
                default -> throw new IllegalStateException("unknown pair accessor step");
            };
        }
        return current;
    }

    private Value stringAppend(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(requireString(argument, "string-append"));
        }
        return immutableString(builder.toString());
    }

    private Value string(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder(arguments.size());
        for (Value argument : arguments) {
            builder.append(requireChar(argument, "string"));
        }
        return immutableString(builder.toString());
    }

    private Value makeString(List<Value> arguments) throws EvalError {
        if (arguments.size() != 1 && arguments.size() != 2) {
            throw new EvalError("make-string expected 1 or 2 argument(s)");
        }

        int length = requireIndex(arguments.get(0), "make-string");
        char fill = arguments.size() == 2 ? requireChar(arguments.get(1), "make-string") : '\0';
        StringBuilder builder = new StringBuilder(length);
        for (int index = 0; index < length; index++) {
            builder.append(fill);
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
                return quoteConverter.convert(expression);
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

    private Value memq(List<Value> arguments) throws EvalError {
        return memberBy(arguments, "memq", ValueSupport::eqValues);
    }

    private Value memv(List<Value> arguments) throws EvalError {
        return memberBy(arguments, "memv", ValueSupport::eqValues);
    }

    private Value member(List<Value> arguments) throws EvalError {
        return memberBy(arguments, "member", ValueSupport::equalValues);
    }

    private Value memberBy(List<Value> arguments, String name,
            BiPredicate<Value, Value> predicate) throws EvalError {
        requireExactArgs(name, arguments, 2);
        Value target = arguments.get(0);
        Value current = arguments.get(1);
        while (current instanceof PairValue pairValue) {
            if (predicate.test(target, pairValue.car())) {
                return current;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(name + " expects a proper list");
        }
        return FALSE;
    }

    private Value assq(List<Value> arguments) throws EvalError {
        return assocBy(arguments, "assq", ValueSupport::eqValues);
    }

    private Value assv(List<Value> arguments) throws EvalError {
        return assocBy(arguments, "assv", ValueSupport::eqValues);
    }

    private Value assoc(List<Value> arguments) throws EvalError {
        return assocBy(arguments, "assoc", ValueSupport::equalValues);
    }

    private Value assocBy(List<Value> arguments, String name,
            BiPredicate<Value, Value> predicate) throws EvalError {
        requireExactArgs(name, arguments, 2);

        Value key = arguments.get(0);
        Value current = arguments.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            if (!(entry instanceof PairValue entryPair)) {
                throw new EvalError(name + " expects an association list");
            }
            if (predicate.test(key, entryPair.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(name + " expects a proper list");
        }
        return FALSE;
    }

    private Value reverse(List<Value> arguments) throws EvalError {
        requireExactArgs("reverse", arguments, 1);
        List<Value> elements = requireProperList(arguments.get(0), "reverse");
        Value result = EMPTY_LIST;
        for (Value element : elements) {
            result = new PairValue(element, result);
        }
        return result;
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
        return procedureInvoker.apply(operator, appliedArguments);
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

    private Value gcd(List<Value> arguments) throws EvalError {
        long result = 0L;
        for (Value argument : arguments) {
            result = gcdLong(result, requireInt(argument, "gcd"));
        }
        return new IntValue(Math.abs(result));
    }

    private Value lcm(List<Value> arguments) throws EvalError {
        long result = 1L;
        for (Value argument : arguments) {
            long value = requireInt(argument, "lcm");
            if (result == 0L || value == 0L) {
                result = 0L;
            } else {
                result = Math.abs((result / gcdLong(result, value)) * value);
            }
        }
        return new IntValue(result);
    }

    private Value truncate(List<Value> arguments) throws EvalError {
        requireExactArgs("truncate", arguments, 1);
        Value value = requireNumericValue(arguments.get(0), "truncate");
        return switch (value) {
            case IntValue ignored -> value;
            case RationalValue rationalValue ->
                    new IntValue(rationalValue.numerator() / rationalValue.denominator());
            case InexactValue inexactValue -> new InexactValue(
                    inexactValue.value() < 0.0
                            ? Math.ceil(inexactValue.value())
                            : Math.floor(inexactValue.value()));
            default -> throw new IllegalStateException("non-numeric value");
        };
    }

    private Value round(List<Value> arguments) throws EvalError {
        requireExactArgs("round", arguments, 1);
        Value value = requireNumericValue(arguments.get(0), "round");
        return switch (value) {
            case IntValue ignored -> value;
            case RationalValue rationalValue ->
                    new IntValue(roundExact(rationalValue.numerator(), rationalValue.denominator()));
            case InexactValue inexactValue -> new InexactValue(Math.rint(inexactValue.value()));
            default -> throw new IllegalStateException("non-numeric value");
        };
    }

    private long roundExact(long numerator, long denominator) {
        long floor = Math.floorDiv(numerator, denominator);
        long remainder = numerator - floor * denominator;
        long doubled = remainder * 2L;
        if (doubled < denominator) {
            return floor;
        }
        if (doubled > denominator) {
            return floor + 1L;
        }
        return (floor & 1L) == 0L ? floor : floor + 1L;
    }

    private long gcdLong(long left, long right) {
        long a = Math.abs(left);
        long b = Math.abs(right);
        if (a == 0L) {
            return b;
        }
        while (b != 0L) {
            long next = a % b;
            a = b;
            b = next;
        }
        return a;
    }
}
