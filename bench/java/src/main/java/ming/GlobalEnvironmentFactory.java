package ming;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

final class GlobalEnvironmentFactory {
    private final Evaluator evaluator;
    private final CollectionProcedures collections;
    private final Environment env = new Environment(null);

    private GlobalEnvironmentFactory(Evaluator evaluator) {
        this.evaluator = evaluator;
        this.collections = evaluator.collectionProcedures();
    }

    static Environment create(Evaluator evaluator) {
        GlobalEnvironmentFactory factory = new GlobalEnvironmentFactory(evaluator);
        factory.installNumericProcedures();
        factory.installBooleanProcedures();
        factory.installListProcedures();
        factory.installVectorProcedures();
        factory.installEquivalenceProcedures();
        factory.installTypePredicates();
        factory.installOutputProcedures();
        factory.installStringProcedures();
        factory.installCharacterProcedures();
        factory.installExactnessProcedures();
        factory.installUtilityProcedures();
        return factory.env;
    }

    private void define(String name, BuiltinAction action) {
        env.define(name, evaluator.builtin(name, action));
    }

    private void defineValue(String name, Value value) {
        env.define(name, value);
    }

    private void installNumericProcedures() {
        define("+", evaluator::addNumbers);
        define("-", evaluator::subtractNumbers);
        define("*", evaluator::multiplyNumbers);
        define("/", evaluator::divideNumbers);
        define("abs", evaluator::absBuiltin);
        define("quotient", args -> new IntValue(evaluator.quotient(args)));
        define("remainder", args -> new IntValue(evaluator.remainder(args)));
        define("modulo", args -> new IntValue(evaluator.modulo(args)));
        define("min", evaluator::minBuiltin);
        define("max", evaluator::maxBuiltin);
        define("gcd", evaluator::gcdBuiltin);
        define("lcm", evaluator::lcmBuiltin);
        define("truncate", evaluator::truncateBuiltin);
        define("round", evaluator::roundBuiltin);
        define("expt", args -> new IntValue(evaluator.expt(args)));
        define("<", args -> BoolValue.of(
                evaluator.compareIncreasing(args, Comparison.STRICTLY_LESS)));
        define(">", args -> BoolValue.of(
                evaluator.compareIncreasing(args, Comparison.STRICTLY_GREATER)));
        define("=", args -> BoolValue.of(evaluator.compareIncreasing(args, Comparison.EQUAL)));
        define("<=", args -> BoolValue.of(
                evaluator.compareIncreasing(args, Comparison.LESS_OR_EQUAL)));
        define(">=", args -> BoolValue.of(
                evaluator.compareIncreasing(args, Comparison.GREATER_OR_EQUAL)));
        define("zero?", args -> evaluator.signPredicate("zero?", args, 0));
        define("positive?", args -> evaluator.signPredicate("positive?", args, 1));
        define("negative?", args -> evaluator.signPredicate("negative?", args, -1));
        define("odd?", args -> {
            evaluator.requireArity("odd?", args.size(), 1);
            return BoolValue.of(evaluator.expectInt(args.getFirst()) % 2 != 0);
        });
        define("even?", args -> {
            evaluator.requireArity("even?", args.size(), 1);
            return BoolValue.of(evaluator.expectInt(args.getFirst()) % 2 == 0);
        });
    }

    private void installBooleanProcedures() {
        define("not", args -> {
            evaluator.requireArity("not", args.size(), 1);
            return BoolValue.of(!evaluator.isTruthy(args.getFirst()));
        });
    }

    private void installListProcedures() {
        define("cons", args -> {
            evaluator.requireArity("cons", args.size(), 2);
            return new PairValue(args.get(0), args.get(1));
        });
        define("car", args -> {
            evaluator.requireArity("car", args.size(), 1);
            return evaluator.expectPair(args.getFirst()).car();
        });
        define("cdr", args -> {
            evaluator.requireArity("cdr", args.size(), 1);
            return evaluator.expectPair(args.getFirst()).cdr();
        });
        define("set-car!", collections::setCarBuiltin);
        define("set-cdr!", collections::setCdrBuiltin);
        define("null?", args -> {
            evaluator.requireArity("null?", args.size(), 1);
            return BoolValue.of(args.getFirst() instanceof EmptyListValue);
        });
        define("list", collections::makeList);
        define("length", args -> {
            evaluator.requireArity("length", args.size(), 1);
            return new IntValue(collections.lengthOfList(args.getFirst()));
        });
        define("list-ref", collections::listRef);
        define("list-tail", collections::listTailBuiltin);
        define("reverse", collections::reverseBuiltin);
        define("list?", args -> {
            evaluator.requireArity("list?", args.size(), 1);
            return BoolValue.of(collections.isProperList(args.getFirst()));
        });
        define("append", collections::appendLists);
        define("apply", collections::applyBuiltin);
        define("map", collections::mapBuiltin);
        define("for-each", collections::forEachBuiltin);
        define("member", collections::memberBuiltin);
        define("assv", collections::assvBuiltin);
        define("assoc", collections::assocBuiltin);
        installCxrProcedures();
    }

    private void installCxrProcedures() {
        String[] names = {
                "caar", "cadr", "cdar", "cddr",
                "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar", "cdddr",
                "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr",
                "caddar", "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr",
                "cddaar", "cddadr", "cdddar", "cddddr"
        };
        for (String name : names) {
            define(name, args -> collections.cxrBuiltin(name, args));
        }
    }

    private void installVectorProcedures() {
        define("vector", args -> new VectorValue(args));
        define("make-vector", collections::makeVectorBuiltin);
        define("vector-ref", collections::vectorRefBuiltin);
        define("vector-set!", collections::vectorSetBuiltin);
        define("vector-length", args -> {
            evaluator.requireArity("vector-length", args.size(), 1);
            return new IntValue(evaluator.expectVectorValue(args.getFirst()).length());
        });
        define("vector->list", args -> {
            evaluator.requireArity("vector->list", args.size(), 1);
            return collections.makeList(evaluator.expectVectorValue(args.getFirst()).elements());
        });
        define("list->vector", args -> {
            evaluator.requireArity("list->vector", args.size(), 1);
            return new VectorValue(collections.listElements(args.getFirst()));
        });
    }

    private void installEquivalenceProcedures() {
        define("eq?", args -> {
            evaluator.requireArity("eq?", args.size(), 2);
            return BoolValue.of(evaluator.isEq(args.get(0), args.get(1)));
        });
        define("eqv?", args -> {
            evaluator.requireArity("eqv?", args.size(), 2);
            return BoolValue.of(evaluator.isEqv(args.get(0), args.get(1)));
        });
        define("equal?", args -> {
            evaluator.requireArity("equal?", args.size(), 2);
            return BoolValue.of(evaluator.isEqual(args.get(0), args.get(1)));
        });
    }

    private void installTypePredicates() {
        define("string?", args -> evaluator.typePredicate(
                "string?", args, value -> value instanceof StringValue));
        define("number?", args -> evaluator.typePredicate(
                "number?", args, NumericSupport::isNumber));
        define("boolean?", args -> evaluator.typePredicate(
                "boolean?", args, value -> value instanceof BoolValue));
        define("pair?", args -> evaluator.typePredicate(
                "pair?", args, value -> value instanceof PairValue));
        define("vector?", args -> evaluator.typePredicate(
                "vector?", args, value -> value instanceof VectorValue));
        define("symbol?", args -> evaluator.typePredicate(
                "symbol?", args, value -> value instanceof SymbolValue));
        define("procedure?", args -> {
            evaluator.requireArity("procedure?", args.size(), 1);
            return BoolValue.of(args.getFirst() instanceof ProcedureValue);
        });
        define("integer?", args -> evaluator.typePredicate(
                "integer?", args, NumericSupport::isInteger));
        define("rational?", args -> evaluator.typePredicate(
                "rational?", args, NumericSupport::isRational));
        define("exact?", args -> evaluator.typePredicate(
                "exact?", args, NumericSupport::isExact));
        define("inexact?", args -> evaluator.typePredicate(
                "inexact?", args, NumericSupport::isInexact));
    }

    private void installOutputProcedures() {
        define("display", args -> {
            evaluator.requireArity("display", args.size(), 1);
            evaluator.appendOutput(evaluator.renderForDisplay(args.getFirst()));
            return VoidValue.INSTANCE;
        });
        define("write", args -> {
            evaluator.requireArity("write", args.size(), 1);
            evaluator.appendOutput(args.getFirst().render());
            return VoidValue.INSTANCE;
        });
        define("newline", args -> {
            evaluator.requireArity("newline", args.size(), 0);
            evaluator.appendOutput("\n");
            return VoidValue.INSTANCE;
        });
    }

    private void installStringProcedures() {
        define("make-string", args -> {
            if (args.size() < 1 || args.size() > 2) {
                throw new EvalError(
                        "wrong number of arguments for make-string: expected 1 or 2, got "
                                + args.size());
            }

            int length = evaluator.expectIndex(args.getFirst(), "make-string");
            char fill = args.size() == 2 ? evaluator.expectChar(args.get(1)) : '\0';
            StringBuilder builder = new StringBuilder(length);
            for (int index = 0; index < length; index++) {
                builder.append(fill);
            }
            return new StringValue(builder.toString());
        });
        define("string", args -> {
            StringBuilder builder = new StringBuilder(args.size());
            for (Value arg : args) {
                builder.append(evaluator.expectChar(arg));
            }
            return new StringValue(builder.toString());
        });
        define("string-append", args -> new StringValue(evaluator.stringAppend(args)));
        define("string-length", args -> {
            evaluator.requireArity("string-length", args.size(), 1);
            return new IntValue(evaluator.expectString(args.getFirst()).length());
        });
        define("substring", args -> {
            evaluator.requireArity("substring", args.size(), 3);
            String value = evaluator.expectString(args.get(0));
            int start = evaluator.expectIndex(args.get(1), "substring");
            int end = evaluator.expectIndex(args.get(2), "substring");
            if (start > end || end > value.length()) {
                throw new EvalError("substring indices out of range");
            }
            return new StringValue(value.substring(start, end));
        });
        define("string->number", args -> {
            evaluator.requireArity("string->number", args.size(), 1);
            return evaluator.stringToNumber(evaluator.expectString(args.getFirst()));
        });
        define("number->string", args -> {
            evaluator.requireArity("number->string", args.size(), 1);
            return new StringValue(evaluator.expectNumber(args.getFirst()).render());
        });
        define("symbol->string", args -> {
            evaluator.requireArity("symbol->string", args.size(), 1);
            return new StringValue(evaluator.expectSymbol(args.getFirst()));
        });
        define("string->symbol", args -> {
            evaluator.requireArity("string->symbol", args.size(), 1);
            return new SymbolValue(evaluator.expectString(args.getFirst()));
        });
        define("string-ref", args -> {
            evaluator.requireArity("string-ref", args.size(), 2);
            StringValue value = evaluator.expectStringValue(args.get(0));
            int index = evaluator.expectIndex(args.get(1), "string-ref");
            if (index >= value.length()) {
                throw new EvalError("string-ref index out of range");
            }
            return new CharValue(value.charAt(index));
        });
        define("string->list", args -> {
            evaluator.requireArity("string->list", args.size(), 1);

            String value = evaluator.expectString(args.getFirst());
            List<Value> characters = new ArrayList<>(value.length());
            for (int index = 0; index < value.length(); index++) {
                characters.add(new CharValue(value.charAt(index)));
            }
            return collections.makeList(characters);
        });
        define("list->string", args -> {
            evaluator.requireArity("list->string", args.size(), 1);

            List<Value> elements = collections.listElements(args.getFirst());
            StringBuilder builder = new StringBuilder(elements.size());
            for (Value element : elements) {
                builder.append(evaluator.expectChar(element));
            }
            return new StringValue(builder.toString());
        });
        define("string-copy", args -> {
            evaluator.requireArity("string-copy", args.size(), 1);
            return evaluator.expectStringValue(args.getFirst()).copy(true);
        });
        define("string=?", args -> BoolValue.of(
                evaluator.compareStrings(args, "string=?", StringComparison.EQUAL)));
        define("string<?", args -> BoolValue.of(
                evaluator.compareStrings(args, "string<?", StringComparison.LESS)));
        define("string>?", args -> {
            evaluator.requireAtLeast("string>?", args.size(), 2);

            String previous = evaluator.expectString(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                String current = evaluator.expectString(args.get(index));
                if (previous.compareTo(current) <= 0) {
                    return BoolValue.FALSE;
                }
                previous = current;
            }
            return BoolValue.TRUE;
        });
        define("string<=?", args -> {
            evaluator.requireAtLeast("string<=?", args.size(), 2);

            String previous = evaluator.expectString(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                String current = evaluator.expectString(args.get(index));
                if (previous.compareTo(current) > 0) {
                    return BoolValue.FALSE;
                }
                previous = current;
            }
            return BoolValue.TRUE;
        });
        define("string>=?", args -> {
            evaluator.requireAtLeast("string>=?", args.size(), 2);

            String previous = evaluator.expectString(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                String current = evaluator.expectString(args.get(index));
                if (previous.compareTo(current) < 0) {
                    return BoolValue.FALSE;
                }
                previous = current;
            }
            return BoolValue.TRUE;
        });
        define("string-ci=?", args -> {
            evaluator.requireAtLeast("string-ci=?", args.size(), 2);

            String previous = evaluator.expectString(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                String current = evaluator.expectString(args.get(index));
                if (!previous.equalsIgnoreCase(current)) {
                    return BoolValue.FALSE;
                }
                previous = current;
            }
            return BoolValue.TRUE;
        });
        define("string-upcase", args -> {
            evaluator.requireArity("string-upcase", args.size(), 1);
            return new StringValue(evaluator.expectString(args.getFirst()).toUpperCase(
                    Locale.ROOT));
        });
        define("string-downcase", args -> {
            evaluator.requireArity("string-downcase", args.size(), 1);
            return new StringValue(evaluator.expectString(args.getFirst()).toLowerCase(
                    Locale.ROOT));
        });
        define("string-set!", args -> {
            evaluator.requireArity("string-set!", args.size(), 3);
            StringValue value = evaluator.expectStringValue(args.get(0));
            int index = evaluator.expectIndex(args.get(1), "string-set!");
            if (index >= value.length()) {
                throw new EvalError("string-set! index out of range");
            }
            value.setCharAt(index, evaluator.expectChar(args.get(2)));
            return VoidValue.INSTANCE;
        });
    }

    private void installCharacterProcedures() {
        define("char?", args -> evaluator.typePredicate(
                "char?", args, value -> value instanceof CharValue));
        define("char->integer", args -> {
            evaluator.requireArity("char->integer", args.size(), 1);
            return new IntValue(evaluator.expectChar(args.getFirst()));
        });
        define("integer->char", args -> {
            evaluator.requireArity("integer->char", args.size(), 1);

            int codePoint = evaluator.expectInt(args.getFirst());
            if (!Character.isValidCodePoint(codePoint)
                    || !Character.isBmpCodePoint(codePoint)
                    || Character.isSurrogate((char) codePoint)) {
                throw new EvalError("integer->char code point out of range");
            }
            return new CharValue((char) codePoint);
        });
        define("char-alphabetic?", args -> {
            evaluator.requireArity("char-alphabetic?", args.size(), 1);
            return BoolValue.of(Character.isLetter(evaluator.expectChar(args.getFirst())));
        });
        define("char-numeric?", args -> {
            evaluator.requireArity("char-numeric?", args.size(), 1);
            return BoolValue.of(Character.isDigit(evaluator.expectChar(args.getFirst())));
        });
        define("char-upcase", args -> {
            evaluator.requireArity("char-upcase", args.size(), 1);
            return new CharValue(Character.toUpperCase(evaluator.expectChar(args.getFirst())));
        });
        define("char-downcase", args -> {
            evaluator.requireArity("char-downcase", args.size(), 1);
            return new CharValue(Character.toLowerCase(evaluator.expectChar(args.getFirst())));
        });
        define("char=?", args -> BoolValue.of(
                evaluator.compareChars(args, "char=?", CharComparison.EQUAL)));
        define("char<?", args -> BoolValue.of(
                evaluator.compareChars(args, "char<?", CharComparison.LESS)));
    }

    private void installExactnessProcedures() {
        define("exact->inexact", args -> {
            evaluator.requireArity("exact->inexact", args.size(), 1);
            return evaluator.exactToInexact(evaluator.expectNumber(args.getFirst()));
        });
        define("inexact->exact", args -> {
            evaluator.requireArity("inexact->exact", args.size(), 1);
            return NumericSupport.inexactToExact(args.getFirst());
        });
        define("numerator", evaluator::numeratorBuiltin);
        define("denominator", evaluator::denominatorBuiltin);
    }

    private void installUtilityProcedures() {
        define("error", evaluator::errorBuiltin);
        defineValue("values", new ValuesProcedure("values"));
        defineValue("call-with-values", new CallWithValuesProcedure("call-with-values"));
        defineValue("dynamic-wind", new DynamicWindProcedure("dynamic-wind"));
        defineValue("call/cc", new CallCcProcedure("call/cc"));
        defineValue("call-with-current-continuation",
                new CallCcProcedure("call-with-current-continuation"));
        defineValue("raise", new RaiseProcedure("raise"));
        defineValue("with-exception-handler",
                new WithExceptionHandlerProcedure("with-exception-handler"));
    }
}
