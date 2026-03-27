package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Function;

final class Builtins {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);
    private static final EmptyListValue EMPTY_LIST = EmptyListValue.INSTANCE;
    private static final VoidValue VOID = VoidValue.INSTANCE;

    private final Function<String, StringValue> stringFactory;
    private final StringBuilder output;
    private final ValueEquality equality;
    private final HigherOrderBuiltins higherOrderBuiltins;

    Builtins(
            Function<String, StringValue> stringFactory,
            StringBuilder output,
            ValueEquality equality
    ) {
        this.stringFactory = stringFactory;
        this.output = output;
        this.equality = equality;
        this.higherOrderBuiltins = new HigherOrderBuiltins(this);
    }

    Environment createGlobalEnvironment() {
        Environment env = new Environment(null);
        defineBuiltin(env, "+", this::applyAdd);
        defineBuiltin(env, "-", this::applySubtract);
        defineBuiltin(env, "*", this::applyMultiply);
        defineBuiltin(env, "/", this::applyDivide);
        defineBuiltin(env, "<", this::applyLessThan);
        defineBuiltin(env, ">", this::applyGreaterThan);
        defineBuiltin(env, "=", this::applyNumericEquals);
        defineBuiltin(env, "<=", this::applyLessEqual);
        defineBuiltin(env, ">=", this::applyGreaterEqual);
        defineBuiltin(env, "not", this::applyNot);
        defineBuiltin(env, "display", this::applyDisplay);
        defineBuiltin(env, "write", this::applyWrite);
        defineBuiltin(env, "newline", this::applyNewline);
        defineBuiltin(env, "error", this::applyError);
        defineBuiltin(env, "cons", this::applyCons);
        defineBuiltin(env, "car", this::applyCar);
        defineBuiltin(env, "cdr", this::applyCdr);
        defineBuiltin(env, "cddr", this::applyCddr);
        defineBuiltin(env, "set-car!", this::applySetCar);
        defineBuiltin(env, "set-cdr!", this::applySetCdr);
        defineBuiltin(env, "null?", this::applyNullPredicate);
        defineBuiltin(env, "list", this::applyList);
        defineBuiltin(env, "length", this::applyLength);
        defineBuiltin(env, "append", this::applyAppend);
        defineBuiltin(env, "reverse", this::applyReverse);
        defineBuiltin(env, "apply", this::applyApply, higherOrderBuiltins::invokeApply);
        defineBuiltin(env, "call/cc", higherOrderBuiltins::applyCallWithCurrentContinuation,
                higherOrderBuiltins::invokeCallWithCurrentContinuation);
        defineBuiltin(env, "call-with-current-continuation",
                higherOrderBuiltins::applyCallWithCurrentContinuation,
                higherOrderBuiltins::invokeCallWithCurrentContinuation);
        defineBuiltin(env, "string-append", this::applyStringAppend);
        defineBuiltin(env, "make-string", this::applyMakeString);
        defineBuiltin(env, "string", this::applyString);
        defineBuiltin(env, "string-length", this::applyStringLength);
        defineBuiltin(env, "substring", this::applySubstring);
        defineBuiltin(env, "string-copy", this::applyStringCopy);
        defineBuiltin(env, "string-set!", this::applyStringSet);
        defineBuiltin(env, "string->list", this::applyStringToList);
        defineBuiltin(env, "list->string", this::applyListToString);
        defineBuiltin(env, "string->number", this::applyStringToNumber);
        defineBuiltin(env, "number->string", this::applyNumberToString);
        defineBuiltin(env, "symbol->string", this::applySymbolToString);
        defineBuiltin(env, "string->symbol", this::applyStringToSymbol);
        defineBuiltin(env, "string-ref", this::applyStringRef);
        defineBuiltin(env, "char->integer", this::applyCharToInteger);
        defineBuiltin(env, "integer->char", this::applyIntegerToChar);
        defineBuiltin(env, "char?", this::applyCharPredicate);
        defineBuiltin(env, "string?", this::applyStringPredicate);
        defineBuiltin(env, "number?", this::applyNumberPredicate);
        defineBuiltin(env, "exact?", this::applyExactPredicate);
        defineBuiltin(env, "inexact?", this::applyInexactPredicate);
        defineBuiltin(env, "exact->inexact", this::applyExactToInexact);
        defineBuiltin(env, "inexact->exact", this::applyInexactToExact);
        defineBuiltin(env, "integer?", this::applyIntegerPredicate);
        defineBuiltin(env, "rational?", this::applyRationalPredicate);
        defineBuiltin(env, "numerator", this::applyNumerator);
        defineBuiltin(env, "denominator", this::applyDenominator);
        defineBuiltin(env, "boolean?", this::applyBooleanPredicate);
        defineBuiltin(env, "procedure?", this::applyProcedurePredicate);
        defineBuiltin(env, "pair?", this::applyPairPredicate);
        defineBuiltin(env, "symbol?", this::applySymbolPredicate);
        defineBuiltin(env, "eq?", this::applyEq);
        defineBuiltin(env, "eqv?", this::applyEqv);
        defineBuiltin(env, "equal?", this::applyEqual);
        defineBuiltin(env, "vector", this::applyVector);
        defineBuiltin(env, "make-vector", this::applyMakeVector);
        defineBuiltin(env, "vector-ref", this::applyVectorRef);
        defineBuiltin(env, "vector-set!", this::applyVectorSet);
        defineBuiltin(env, "vector-length", this::applyVectorLength);
        defineBuiltin(env, "vector?", this::applyVectorPredicate);
        defineBuiltin(env, "vector->list", this::applyVectorToList);
        defineBuiltin(env, "list->vector", this::applyListToVector);
        defineBuiltin(env, "map", this::applyMap, higherOrderBuiltins::invokeMap);
        defineBuiltin(env, "for-each", this::applyForEach, higherOrderBuiltins::invokeForEach);
        defineBuiltin(env, "abs", this::applyAbs);
        defineBuiltin(env, "modulo", this::applyModulo);
        defineBuiltin(env, "remainder", this::applyRemainder);
        defineBuiltin(env, "quotient", this::applyQuotient);
        defineBuiltin(env, "min", this::applyMin);
        defineBuiltin(env, "max", this::applyMax);
        defineBuiltin(env, "expt", this::applyExpt);
        defineBuiltin(env, "zero?", this::applyZeroPredicate);
        defineBuiltin(env, "positive?", this::applyPositivePredicate);
        defineBuiltin(env, "negative?", this::applyNegativePredicate);
        defineBuiltin(env, "odd?", this::applyOddPredicate);
        defineBuiltin(env, "even?", this::applyEvenPredicate);
        defineBuiltin(env, "list-ref", this::applyListRef);
        defineBuiltin(env, "list-tail", this::applyListTail);
        defineBuiltin(env, "list?", this::applyListPredicate);
        defineBuiltin(env, "member", this::applyMember);
        defineBuiltin(env, "assoc", this::applyAssoc);
        defineBuiltin(env, "assv", this::applyAssv);
        defineBuiltin(env, "char-alphabetic?", this::applyCharAlphabeticPredicate);
        defineBuiltin(env, "char-numeric?", this::applyCharNumericPredicate);
        defineBuiltin(env, "char-upcase", this::applyCharUpcase);
        defineBuiltin(env, "char-downcase", this::applyCharDowncase);
        defineBuiltin(env, "char=?", this::applyCharEquals);
        defineBuiltin(env, "char<?", this::applyCharLessThan);
        defineBuiltin(env, "string=?", this::applyStringEquals);
        defineBuiltin(env, "string<?", this::applyStringLessThan);
        defineBuiltin(env, "string>?", this::applyStringGreaterThan);
        defineBuiltin(env, "string<=?", this::applyStringLessEqual);
        defineBuiltin(env, "string>=?", this::applyStringGreaterEqual);
        defineBuiltin(env, "string-ci=?", this::applyStringCiEquals);
        defineBuiltin(env, "string-upcase", this::applyStringUpcase);
        defineBuiltin(env, "string-downcase", this::applyStringDowncase);
        defineBuiltin(env, "gcd", this::applyGcd);
        defineBuiltin(env, "lcm", this::applyLcm);
        defineBuiltin(env, "truncate", this::applyTruncate);
        defineBuiltin(env, "round", this::applyRound);
        return env;
    }

    private void defineBuiltin(Environment env, String name, BuiltinInvoker invoker) {
        env.define(name, new BuiltinProcedure(name, invoker));
    }

    private void defineBuiltin(
            Environment env,
            String name,
            BuiltinInvoker invoker,
            MachineBuiltinInvoker machineInvoker
    ) {
        env.define(name, new BuiltinProcedure(name, invoker, machineInvoker));
    }

    private StringValue createStringValue(String value) {
        return stringFactory.apply(value);
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
                ? SchemeNumber.EXACT_ONE.divide(
                        requireNumber(arguments.getFirst(), "/", callLoc),
                        callLoc
                )
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

    private Value applyGreaterEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, ">=", (left, right) -> left.compareTo(right) >= 0);
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

    private Value applyError(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("error", arguments, 1, callLoc);

        StringBuilder message = new StringBuilder(arguments.getFirst().displayRender());
        if (arguments.size() > 1) {
            message.append(": ");
            for (int index = 1; index < arguments.size(); index++) {
                if (index > 1) {
                    message.append(' ');
                }
                message.append(arguments.get(index).render());
            }
        }
        throw error(callLoc, message.toString());
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

    private Value applyCddr(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("cddr", arguments, 1, callLoc);
        return requirePair(requirePair(arguments.getFirst(), "cddr", callLoc).cdr(),
                "cddr", callLoc).cdr();
    }

    private Value applySetCar(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("set-car!", arguments, 2, callLoc);
        requirePair(arguments.getFirst(), "set-car!", callLoc).setCar(arguments.get(1));
        return VOID;
    }

    private Value applySetCdr(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("set-cdr!", arguments, 2, callLoc);
        requirePair(arguments.getFirst(), "set-cdr!", callLoc).setCdr(arguments.get(1));
        return VOID;
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

    private Value applyReverse(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("reverse", arguments, 1, callLoc);

        Value result = EMPTY_LIST;
        for (Value element : requireProperList(arguments.getFirst(), "reverse", callLoc)) {
            result = new PairValue(element, result);
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
        return createStringValue(builder.toString());
    }

    private Value applyMakeString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        if (arguments.size() != 1 && arguments.size() != 2) {
            throw error(callLoc,
                    "make-string expected 1 or 2 arguments but got " + arguments.size());
        }

        int length = requireIndex(arguments.getFirst(), "make-string", callLoc);
        int fillCodePoint = arguments.size() == 2
                ? requireChar(arguments.get(1), "make-string", callLoc).codePoint()
                : ' ';

        StringBuilder builder = new StringBuilder(length);
        for (int index = 0; index < length; index++) {
            builder.appendCodePoint(fillCodePoint);
        }
        return createStringValue(builder.toString());
    }

    private Value applyString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.appendCodePoint(requireChar(argument, "string", callLoc).codePoint());
        }
        return createStringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-length", arguments, 1, callLoc);
        String value = requireString(arguments.getFirst(), "string-length", callLoc);
        int length = value.codePointCount(0, value.length());
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
        return createStringValue(value.substring(startOffset, endOffset));
    }

    private Value applyStringCopy(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-copy", arguments, 1, callLoc);
        return createStringValue(requireString(arguments.getFirst(), "string-copy", callLoc));
    }

    private Value applyStringSet(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-set!", arguments, 3, callLoc);
        StringValue stringValue = requireStringValue(arguments.get(0), "string-set!", callLoc);
        int index = requireIndex(arguments.get(1), "string-set!", callLoc);
        CharValue charValue = requireChar(arguments.get(2), "string-set!", callLoc);
        stringValue.setCodePoint(index, charValue.codePoint(), callLoc);
        return VOID;
    }

    private Value applyStringToList(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string->list", arguments, 1, callLoc);
        String value = requireString(arguments.getFirst(), "string->list", callLoc);
        List<Value> characters = new ArrayList<>(value.codePointCount(0, value.length()));
        for (int offset = 0; offset < value.length(); ) {
            int codePoint = value.codePointAt(offset);
            characters.add(new CharValue(codePoint));
            offset += Character.charCount(codePoint);
        }
        return buildList(characters);
    }

    private Value applyListToString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("list->string", arguments, 1, callLoc);
        List<Value> characters = requireProperList(arguments.getFirst(), "list->string", callLoc);
        StringBuilder builder = new StringBuilder();
        for (Value character : characters) {
            builder.appendCodePoint(requireChar(character, "list->string", callLoc).codePoint());
        }
        return createStringValue(builder.toString());
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
        return createStringValue(requireNumber(arguments.getFirst(), "number->string", callLoc).render());
    }

    private Value applySymbolToString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("symbol->string", arguments, 1, callLoc);
        return createStringValue(requireSymbol(arguments.getFirst(), "symbol->string", callLoc));
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

    private Value applyCharToInteger(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("char->integer", arguments, 1, callLoc);
        int codePoint = requireChar(arguments.getFirst(), "char->integer", callLoc).codePoint();
        return new NumberValue(SchemeNumber.exact(Rational.integer(BigInteger.valueOf(codePoint))));
    }

    private Value applyIntegerToChar(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("integer->char", arguments, 1, callLoc);
        int codePoint = requireIndex(arguments.getFirst(), "integer->char", callLoc);
        if (!Character.isValidCodePoint(codePoint)
                || (codePoint <= Character.MAX_VALUE && Character.isSurrogate((char) codePoint))) {
            throw error(callLoc, "integer->char expects a valid character code point");
        }
        return new CharValue(codePoint);
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

    private Value applyProcedurePredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("procedure?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof Procedure ? TRUE : FALSE;
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
        return equality.eqv(arguments.get(0), arguments.get(1)) ? TRUE : FALSE;
    }

    private Value applyEqv(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("eqv?", arguments, 2, callLoc);
        return equality.eqv(arguments.get(0), arguments.get(1)) ? TRUE : FALSE;
    }

    private Value applyEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("equal?", arguments, 2, callLoc);
        return equality.equal(arguments.get(0), arguments.get(1)) ? TRUE : FALSE;
    }

    private Value applyVector(List<Value> arguments, SourceLoc callLoc) {
        return new VectorValue(arguments);
    }

    private Value applyMakeVector(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        if (arguments.size() != 1 && arguments.size() != 2) {
            throw error(callLoc,
                    "make-vector expected 1 or 2 arguments but got " + arguments.size());
        }

        int length = requireIndex(arguments.getFirst(), "make-vector", callLoc);
        Value fill = arguments.size() == 2 ? arguments.get(1) : VOID;
        List<Value> elements = new ArrayList<>(length);
        for (int index = 0; index < length; index++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    private Value applyVectorRef(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("vector-ref", arguments, 2, callLoc);
        VectorValue vectorValue = requireVector(arguments.get(0), "vector-ref", callLoc);
        int index = requireIndex(arguments.get(1), "vector-ref", callLoc);
        if (index >= vectorValue.size()) {
            throw error(callLoc, "vector-ref index is out of bounds");
        }
        return vectorValue.element(index);
    }

    private Value applyVectorSet(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("vector-set!", arguments, 3, callLoc);
        VectorValue vectorValue = requireVector(arguments.get(0), "vector-set!", callLoc);
        int index = requireIndex(arguments.get(1), "vector-set!", callLoc);
        vectorValue.setElement(index, arguments.get(2), callLoc, "vector-set!");
        return VOID;
    }

    private Value applyVectorLength(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("vector-length", arguments, 1, callLoc);
        VectorValue vectorValue = requireVector(arguments.getFirst(), "vector-length", callLoc);
        return new NumberValue(SchemeNumber.exact(
                Rational.integer(BigInteger.valueOf(vectorValue.size()))
        ));
    }

    private Value applyVectorPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("vector?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof VectorValue ? TRUE : FALSE;
    }

    private Value applyVectorToList(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("vector->list", arguments, 1, callLoc);
        return buildList(requireVector(arguments.getFirst(), "vector->list", callLoc).elements());
    }

    private Value applyListToVector(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("list->vector", arguments, 1, callLoc);
        return new VectorValue(requireProperList(arguments.getFirst(), "list->vector", callLoc));
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

    private Value applyForEach(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("for-each", arguments, 2, callLoc);

        Value procedureValue = arguments.getFirst();
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(callLoc, "for-each expects a procedure as its first argument");
        }

        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int expectedLength = -1;
        for (int index = 1; index < arguments.size(); index++) {
            List<Value> elements = requireProperList(arguments.get(index), "for-each", callLoc);
            if (expectedLength == -1) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw error(callLoc, "for-each expects lists of equal length");
            }
            lists.add(elements);
        }

        for (int elementIndex = 0; elementIndex < expectedLength; elementIndex++) {
            List<Value> appliedArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                appliedArguments.add(list.get(elementIndex));
            }
            procedure.apply(appliedArguments, callLoc);
        }
        return VOID;
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

    private Value applyGcd(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value argument : arguments) {
            result = result.gcd(requireInteger(argument, "gcd", callLoc).abs());
        }
        return new NumberValue(SchemeNumber.exact(Rational.integer(result)));
    }

    private Value applyLcm(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        BigInteger result = BigInteger.ONE;
        for (Value argument : arguments) {
            BigInteger value = requireInteger(argument, "lcm", callLoc).abs();
            if (value.signum() == 0) {
                result = BigInteger.ZERO;
                break;
            }
            result = result.divide(result.gcd(value)).multiply(value).abs();
        }
        return new NumberValue(SchemeNumber.exact(Rational.integer(result)));
    }

    private Value applyTruncate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("truncate", arguments, 1, callLoc);
        SchemeNumber value = requireNumber(arguments.getFirst(), "truncate", callLoc);
        if (value.isExact()) {
            return new NumberValue(SchemeNumber.exact(
                    Rational.integer(truncateExact(value.toExact()))
            ));
        }

        double inexact = value.toDouble();
        double truncated = inexact < 0.0d ? Math.ceil(inexact) : Math.floor(inexact);
        return new NumberValue(SchemeNumber.inexact(truncated));
    }

    private Value applyRound(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("round", arguments, 1, callLoc);
        SchemeNumber value = requireNumber(arguments.getFirst(), "round", callLoc);
        if (value.isExact()) {
            return new NumberValue(SchemeNumber.exact(
                    Rational.integer(roundExact(value.toExact()))
            ));
        }
        return new NumberValue(SchemeNumber.inexact(Math.rint(value.toDouble())));
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

    private Value applyMember(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("member", arguments, 2, callLoc);
        return findMember(
                arguments.get(0),
                arguments.get(1),
                "member",
                callLoc,
                equality::equal
        );
    }

    private Value applyAssoc(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("assoc", arguments, 2, callLoc);
        return findAssociation(
                arguments.get(0),
                arguments.get(1),
                "assoc",
                callLoc,
                equality::equal
        );
    }

    private Value applyAssv(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("assv", arguments, 2, callLoc);
        return findAssociation(arguments.get(0), arguments.get(1), "assv", callLoc, equality::eqv);
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

    private Value applyStringGreaterThan(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureAtLeast("string>?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string>?", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string>?", callLoc);
            if (previous.compareTo(current) <= 0) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringLessEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("string<=?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string<=?", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string<=?", callLoc);
            if (previous.compareTo(current) > 0) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringGreaterEqual(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureAtLeast("string>=?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string>=?", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string>=?", callLoc);
            if (previous.compareTo(current) < 0) {
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
        return createStringValue(
                requireString(arguments.getFirst(), "string-upcase", callLoc).toUpperCase(Locale.ROOT)
        );
    }

    private Value applyStringDowncase(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-downcase", arguments, 1, callLoc);
        return createStringValue(
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

    private VectorValue requireVector(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw error(callLoc, procedureName + " expects a vector");
    }

    List<Value> requireProperListValue(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        return requireProperList(value, procedureName, callLoc);
    }

    Value buildListValue(List<Value> elements) {
        return buildList(elements);
    }

    EvalError errorAt(SourceLoc loc, String message) {
        return error(loc, message);
    }

    private List<Value> requireProperList(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        List<Value> elements = new ArrayList<>();
        Set<PairValue> seen = new HashSet<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw error(callLoc, procedureName + " expects a proper list");
            }
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw error(callLoc, procedureName + " expects a proper list");
    }

    private boolean isProperList(Value value) {
        Value slow = value;
        Value fast = value;

        while (fast instanceof PairValue fastPair) {
            fast = fastPair.cdr();
            if (fast instanceof EmptyListValue) {
                return true;
            }
            if (!(fast instanceof PairValue nextFastPair)) {
                return false;
            }

            fast = nextFastPair.cdr();
            slow = ((PairValue) slow).cdr();
            if (fast == slow) {
                return false;
            }
        }
        return fast instanceof EmptyListValue;
    }

    private Value findAssociation(
            Value key,
            Value list,
            String procedureName,
            SourceLoc callLoc,
            ValueMatcher matcher
    ) throws EvalError {
        Value current = list;
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw error(callLoc, procedureName + " expects a proper list");
            }

            Value entry = pairValue.car();
            PairValue association = requirePair(entry, procedureName, callLoc);
            if (matcher.matches(key, association.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw error(callLoc, procedureName + " expects a proper list");
        }
        return FALSE;
    }

    private Value findMember(
            Value key,
            Value list,
            String procedureName,
            SourceLoc callLoc,
            ValueMatcher matcher
    ) throws EvalError {
        Value current = list;
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw error(callLoc, procedureName + " expects a proper list");
            }

            if (matcher.matches(key, pairValue.car())) {
                return current;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw error(callLoc, procedureName + " expects a proper list");
        }
        return FALSE;
    }

    private BigInteger truncateExact(Rational value) {
        return value.numerator().divide(value.denominator());
    }

    private BigInteger roundExact(Rational value) {
        BigInteger[] quotientAndRemainder = value.numerator().divideAndRemainder(value.denominator());
        BigInteger integerPart = quotientAndRemainder[0];
        BigInteger remainderTwice = quotientAndRemainder[1].abs().multiply(BigInteger.TWO);
        int comparison = remainderTwice.compareTo(value.denominator());
        if (comparison < 0) {
            return integerPart;
        }
        if (comparison > 0) {
            return integerPart.add(value.numerator().signum() >= 0
                    ? BigInteger.ONE
                    : BigInteger.ONE.negate());
        }
        if (!integerPart.testBit(0)) {
            return integerPart;
        }
        return integerPart.add(value.numerator().signum() >= 0
                ? BigInteger.ONE
                : BigInteger.ONE.negate());
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

    private static EvalError error(SourceLoc loc, String message) {
        return SchemeErrors.at(loc, message);
    }

    @FunctionalInterface
    private interface NumberComparison {
        boolean test(SchemeNumber left, SchemeNumber right);
    }

    @FunctionalInterface
    private interface ValueMatcher {
        boolean matches(Value left, Value right);
    }
}
