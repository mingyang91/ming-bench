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
 * Agents implement this class.
 */
public class Evaluator {
    private StringBuilder outputBuffer = new StringBuilder();
    private final Map<String, MacroDefinition> macros = new HashMap<>();
    private int nextHygieneId;

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evaluateWithOutput(input).result().render();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        ProgramEvaluation evaluation = evaluateWithOutput(input);
        return new EvalResult(evaluation.result().render(), evaluation.output());
    }

    private ProgramEvaluation evaluateWithOutput(String input) throws EvalError {
        StringBuilder previousOutput = outputBuffer;
        outputBuffer = new StringBuilder();
        try {
            return new ProgramEvaluation(evaluateProgram(input), outputBuffer.toString());
        } finally {
            outputBuffer = previousOutput;
        }
    }

    private Value evaluateProgram(String input) throws EvalError {
        macros.clear();
        nextHygieneId = 0;

        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input");
        }

        Environment global = createGlobalEnvironment();
        Value result = VOID_VALUE;
        for (Expr expression : expressions) {
            result = eval(expression, global);
        }
        return result;
    }

    private Environment createGlobalEnvironment() {
        Environment env = new Environment(null);
        installBuiltin(env, "+", this::builtinAdd);
        installBuiltin(env, "-", this::builtinSubtract);
        installBuiltin(env, "*", this::builtinMultiply);
        installBuiltin(env, "/", this::builtinDivide);
        installBuiltin(env, "<", (callPos, args) -> builtinComparison(callPos, args, Comparison.LESS_THAN));
        installBuiltin(env, ">", (callPos, args) -> builtinComparison(callPos, args, Comparison.GREATER_THAN));
        installBuiltin(env, "=", (callPos, args) -> builtinComparison(callPos, args, Comparison.EQUAL));
        installBuiltin(env, "<=", (callPos, args) -> builtinComparison(callPos, args, Comparison.LESS_EQUAL));
        installBuiltin(env, ">=", (callPos, args) -> builtinComparison(callPos, args, Comparison.GREATER_EQUAL));
        installBuiltin(env, "cons", this::builtinCons);
        installBuiltin(env, "car", this::builtinCar);
        installBuiltin(env, "cdr", this::builtinCdr);
        installBuiltin(env, "null?", this::builtinNull);
        installBuiltin(env, "list", this::builtinList);
        installBuiltin(env, "length", this::builtinLength);
        installBuiltin(env, "append", this::builtinAppend);
        installBuiltin(env, "display", this::builtinDisplay);
        installBuiltin(env, "write", this::builtinWrite);
        installBuiltin(env, "newline", this::builtinNewline);
        installBuiltin(env, "string?", this::builtinStringPredicate);
        installBuiltin(env, "string-append", this::builtinStringAppend);
        installBuiltin(env, "string-length", this::builtinStringLength);
        installBuiltin(env, "substring", this::builtinSubstring);
        installBuiltin(env, "string-copy", this::builtinStringCopy);
        installBuiltin(env, "string->list", this::builtinStringToList);
        installBuiltin(env, "list->string", this::builtinListToString);
        installBuiltin(env, "string-set!", this::builtinStringSet);
        installBuiltin(env, "string->number", this::builtinStringToNumber);
        installBuiltin(env, "number->string", this::builtinNumberToString);
        installBuiltin(env, "symbol->string", this::builtinSymbolToString);
        installBuiltin(env, "string->symbol", this::builtinStringToSymbol);
        installBuiltin(env, "string-ref", this::builtinStringRef);
        installBuiltin(env, "char->integer", this::builtinCharToInteger);
        installBuiltin(env, "integer->char", this::builtinIntegerToChar);
        installBuiltin(env, "number?", this::builtinNumberPredicate);
        installBuiltin(env, "integer?", this::builtinIntegerPredicate);
        installBuiltin(env, "rational?", this::builtinRationalPredicate);
        installBuiltin(env, "exact?", this::builtinExactPredicate);
        installBuiltin(env, "inexact?", this::builtinInexactPredicate);
        installBuiltin(env, "exact->inexact", this::builtinExactToInexact);
        installBuiltin(env, "inexact->exact", this::builtinInexactToExact);
        installBuiltin(env, "numerator", this::builtinNumerator);
        installBuiltin(env, "denominator", this::builtinDenominator);
        installBuiltin(env, "boolean?", this::builtinBooleanPredicate);
        installBuiltin(env, "char?", this::builtinCharPredicate);
        installBuiltin(env, "pair?", this::builtinPairPredicate);
        installBuiltin(env, "procedure?", this::builtinProcedurePredicate);
        installBuiltin(env, "symbol?", this::builtinSymbolPredicate);
        installBuiltin(env, "eq?", this::builtinEq);
        installBuiltin(env, "eqv?", this::builtinEqv);
        installBuiltin(env, "equal?", this::builtinEqual);
        installBuiltin(env, "not", this::builtinNot);
        installBuiltin(env, "apply", this::builtinApply);
        installBuiltin(env, "map", this::builtinMap);
        installBuiltin(env, "vector", this::builtinVector);
        installBuiltin(env, "make-vector", this::builtinMakeVector);
        installBuiltin(env, "vector-ref", this::builtinVectorRef);
        installBuiltin(env, "vector-set!", this::builtinVectorSet);
        installBuiltin(env, "vector-length", this::builtinVectorLength);
        installBuiltin(env, "vector?", this::builtinVectorPredicate);
        installBuiltin(env, "vector->list", this::builtinVectorToList);
        installBuiltin(env, "list->vector", this::builtinListToVector);
        installBuiltin(env, "abs", this::builtinAbs);
        installBuiltin(env, "modulo", this::builtinModulo);
        installBuiltin(env, "remainder", this::builtinRemainder);
        installBuiltin(env, "quotient", this::builtinQuotient);
        installBuiltin(env, "min", this::builtinMin);
        installBuiltin(env, "max", this::builtinMax);
        installBuiltin(env, "expt", this::builtinExpt);
        installBuiltin(env, "zero?", this::builtinZeroPredicate);
        installBuiltin(env, "positive?", this::builtinPositivePredicate);
        installBuiltin(env, "negative?", this::builtinNegativePredicate);
        installBuiltin(env, "odd?", this::builtinOddPredicate);
        installBuiltin(env, "even?", this::builtinEvenPredicate);
        installBuiltin(env, "list?", this::builtinListPredicate);
        installBuiltin(env, "list-ref", this::builtinListRef);
        installBuiltin(env, "list-tail", this::builtinListTail);
        installBuiltin(env, "assoc", this::builtinAssoc);
        installBuiltin(env, "char-alphabetic?", this::builtinCharAlphabeticPredicate);
        installBuiltin(env, "char-numeric?", this::builtinCharNumericPredicate);
        installBuiltin(env, "char-upcase", this::builtinCharUpcase);
        installBuiltin(env, "char-downcase", this::builtinCharDowncase);
        installBuiltin(env, "char=?", this::builtinCharEquals);
        installBuiltin(env, "char<?", this::builtinCharLessThan);
        installBuiltin(env, "string=?", this::builtinStringEquals);
        installBuiltin(env, "string<?", this::builtinStringLessThan);
        installBuiltin(env, "string-ci=?", this::builtinStringCaseInsensitiveEquals);
        installBuiltin(env, "string-upcase", this::builtinStringUpcase);
        installBuiltin(env, "string-downcase", this::builtinStringDowncase);
        return env;
    }

    private void installBuiltin(Environment env, String name, BuiltinImplementation implementation) {
        env.define(name, new BuiltinProcedure(name, implementation));
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        return eval(expression, env, false);
    }

    private Value eval(Expr expression, Environment env, boolean tailPosition) throws EvalError {
        try {
            if (expression instanceof NumberExpr numberExpr) {
                return numberExpr.value();
            }
            if (expression instanceof BoolExpr boolExpr) {
                return boolValue(boolExpr.value());
            }
            if (expression instanceof StringExpr stringExpr) {
                return new StringValue(stringExpr.value());
            }
            if (expression instanceof CharExpr charExpr) {
                return new CharValue(charExpr.value());
            }
            if (expression instanceof VectorExpr vectorExpr) {
                return quoteVector(vectorExpr.elements());
            }
            if (expression instanceof SymbolExpr symbolExpr) {
                return env.lookup(symbolExpr.name());
            }
            if (expression instanceof ListExpr listExpr) {
                return evalList(listExpr, env, tailPosition);
            }
            throw new IllegalStateException("unknown expression type");
        } catch (EvalError error) {
            throw withPosition(error, expression.pos());
        }
    }

    private Value evalList(ListExpr listExpr, Environment env, boolean tailPosition) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        if (operatorExpr instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            return switch (name) {
                case "and" -> evalAnd(arguments, env, tailPosition);
                case "begin" -> evalBegin(arguments, env, tailPosition);
                case "case" -> evalCase(arguments, env, tailPosition);
                case "case-lambda" -> evalCaseLambda(arguments, env);
                case "cond" -> evalCond(arguments, env, tailPosition);
                case "do" -> evalDo(arguments, env, tailPosition);
                case "or" -> evalOr(arguments, env, tailPosition);
                case "define" -> evalDefine(arguments, env);
                case "define-record-type" -> evalDefineRecordType(arguments, env);
                case "define-syntax" -> evalDefineSyntax(arguments, env);
                case "if" -> evalIf(arguments, env, tailPosition);
                case "let" -> evalLet(arguments, env, tailPosition);
                case "letrec" -> evalLetrec(arguments, env, false, tailPosition);
                case "letrec*" -> evalLetrec(arguments, env, true, tailPosition);
                case "lambda" -> evalLambda(arguments, env);
                case "quote" -> evalQuote(arguments);
                case "set!" -> evalSet(arguments, env);
                default -> {
                    MacroDefinition macroDefinition = macros.get(name);
                    if (macroDefinition != null) {
                        yield evalMacroInvocation(macroDefinition, elements, listExpr.pos(), env, tailPosition);
                    }
                    yield apply(
                            eval(operatorExpr, env),
                            operatorExpr.pos(),
                            evalArguments(arguments, env),
                            listExpr.pos(),
                            tailPosition);
                }
            };
        }
        return apply(
                eval(operatorExpr, env),
                operatorExpr.pos(),
                evalArguments(arguments, env),
                listExpr.pos(),
                tailPosition);
    }

    private List<LocatedValue> evalArguments(List<Expr> arguments, Environment env) throws EvalError {
        List<LocatedValue> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(new LocatedValue(eval(argument, env), argument.pos()));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        Value result = TRUE_VALUE;
        for (int i = 0; i < arguments.size(); i++) {
            result = eval(arguments.get(i), env, tailPosition && i == arguments.size() - 1);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        Value result = FALSE_VALUE;
        for (int i = 0; i < arguments.size(); i++) {
            result = eval(arguments.get(i), env, tailPosition && i == arguments.size() - 1);
            if (isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalBegin(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        return evalSequence(arguments, env, tailPosition);
    }

    private Value evalCase(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("invalid case");
        }

        Value key = eval(arguments.get(0), env);
        for (int i = 1; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof ListExpr clauseExprList) || clauseExprList.elements().isEmpty()) {
                throw new EvalError("invalid case");
            }
            List<Expr> clause = clauseExprList.elements();

            Expr selectorExpr = clause.get(0);
            boolean isElseClause = selectorExpr instanceof SymbolExpr symbolExpr
                    && symbolExpr.name().equals("else");
            if (isElseClause) {
                if (i != arguments.size() - 1) {
                    throw new EvalError("invalid case");
                }
                return clause.size() == 1
                        ? VOID_VALUE
                        : evalSequence(clause.subList(1, clause.size()), env, tailPosition);
            }
            if (!(selectorExpr instanceof ListExpr datumExprs)) {
                throw new EvalError("invalid case");
            }

            for (Expr datumExpr : datumExprs.elements()) {
                if (eqvValues(key, quote(datumExpr))) {
                    return clause.size() == 1
                            ? VOID_VALUE
                            : evalSequence(clause.subList(1, clause.size()), env, tailPosition);
                }
            }
        }
        return VOID_VALUE;
    }

    private Value evalCond(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        for (int i = 0; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof ListExpr clauseExprList) || clauseExprList.elements().isEmpty()) {
                throw new EvalError("invalid cond");
            }
            List<Expr> clause = clauseExprList.elements();

            Expr testExpr = clause.get(0);
            boolean isElseClause = testExpr instanceof SymbolExpr symbolExpr
                    && symbolExpr.name().equals("else");
            if (isElseClause) {
                if (i != arguments.size() - 1) {
                    throw new EvalError("invalid cond");
                }
                return clause.size() == 1
                        ? TRUE_VALUE
                        : evalSequence(clause.subList(1, clause.size()), env, tailPosition);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                return clause.size() == 1
                        ? testValue
                        : evalSequence(clause.subList(1, clause.size()), env, tailPosition);
            }
        }
        return VOID_VALUE;
    }

    private Value evalDefine(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("invalid define");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            if (arguments.size() != 2) {
                throw new EvalError("invalid define");
            }
            env.define(name, eval(arguments.get(1), env));
            return VOID_VALUE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("invalid define");
            }
            if (arguments.size() < 2) {
                throw new EvalError("invalid define");
            }
            ParameterSpec parameters = parseParameters(signature.subList(1, signature.size()));
            String name = nameExpr.name();
            env.define(name, new LambdaProcedure(name, parameters, copyExprs(arguments.subList(1, arguments.size())), env));
            return VOID_VALUE;
        }

        throw new EvalError("invalid define");
    }

    private Value evalIf(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("wrong argument count for if");
        }

        if (isTruthy(eval(arguments.get(0), env))) {
            return eval(arguments.get(1), env, tailPosition);
        }
        if (arguments.size() == 3) {
            return eval(arguments.get(2), env, tailPosition);
        }
        return VOID_VALUE;
    }

    private Value evalDefineSyntax(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() != 2 || !(arguments.get(0) instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("invalid define-syntax");
        }

        macros.put(symbolExpr.name(), parseMacroDefinition(symbolExpr.name(), arguments.get(1), env));
        return VOID_VALUE;
    }

    private Value evalDefineRecordType(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() < 3) {
            throw new EvalError("invalid define-record-type");
        }

        String typeName = requireSymbolExpr(arguments.get(0), "invalid define-record-type");
        RecordConstructorSpec constructor = parseRecordConstructorSpec(arguments.get(1));
        String predicateName = requireSymbolExpr(arguments.get(2), "invalid define-record-type");

        RecordType recordType;
        try {
            recordType = new RecordType(typeName, constructor.fields());
        } catch (IllegalArgumentException e) {
            throw new EvalError("invalid define-record-type");
        }

        env.define(constructor.name(), new BuiltinProcedure(
                constructor.name(),
                (callPos, callArguments) -> constructRecord(callPos, callArguments, constructor.name(), recordType)));
        env.define(predicateName, new BuiltinProcedure(
                predicateName,
                (callPos, callArguments) -> recordPredicate(callPos, callArguments, predicateName, recordType)));

        for (int i = 3; i < arguments.size(); i++) {
            RecordAccessorSpec accessor = parseRecordAccessorSpec(arguments.get(i));
            Integer fieldIndex = recordType.fieldIndex(accessor.fieldName());
            if (fieldIndex == null) {
                throw new EvalError("invalid define-record-type");
            }
            env.define(accessor.accessorName(), new BuiltinProcedure(
                    accessor.accessorName(),
                    (callPos, callArguments) -> recordAccessor(
                            callPos,
                            callArguments,
                            accessor.accessorName(),
                            recordType,
                            fieldIndex)));
        }
        return VOID_VALUE;
    }

    private Value evalMacroInvocation(MacroDefinition macroDefinition, List<Expr> elements,
                                      SourcePos listPos, Environment env, boolean tailPosition) throws EvalError {
        MacroExpansion expansion = expandMacro(macroDefinition, new ListExpr(new ArrayList<>(elements), listPos));
        Environment macroEnv = new Environment(env);
        for (CaptureBinding binding : expansion.captureBindings()) {
            macroEnv.define(binding.name(), binding.value());
        }
        return eval(expansion.expr(), macroEnv, tailPosition);
    }

    private MacroDefinition parseMacroDefinition(String name, Expr rulesExpr, Environment env)
            throws EvalError {
        if (!(rulesExpr instanceof ListExpr partsExpr)) {
            throw new EvalError("invalid define-syntax");
        }
        List<Expr> parts = partsExpr.elements();
        if (parts.size() < 3 || !(parts.get(0) instanceof SymbolExpr keywordExpr)
                || !keywordExpr.name().equals("syntax-rules")) {
            throw new EvalError("invalid define-syntax");
        }
        if (!(parts.get(1) instanceof ListExpr literalExprs)) {
            throw new EvalError("invalid define-syntax");
        }

        Set<String> literals = new HashSet<>();
        for (Expr literalExpr : literalExprs.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw new EvalError("invalid define-syntax");
            }
            literals.add(literalSymbol.name());
        }

        List<MacroRule> rules = new ArrayList<>(parts.size() - 2);
        for (int i = 2; i < parts.size(); i++) {
            Expr ruleExpr = parts.get(i);
            if (!(ruleExpr instanceof ListExpr ruleParts) || ruleParts.elements().size() != 2) {
                throw new EvalError("invalid define-syntax");
            }
            rules.add(new MacroRule(ruleParts.elements().get(0), ruleParts.elements().get(1)));
        }

        Set<String> definitionMacros = new HashSet<>(macros.keySet());
        definitionMacros.add(name);
        return new MacroDefinition(name, literals, rules, env, definitionMacros);
    }

    private MacroExpansion expandMacro(MacroDefinition macroDefinition, Expr callExpr) throws EvalError {
        for (MacroRule rule : macroDefinition.rules()) {
            MacroBindings bindings = matchMacroPattern(
                    rule.pattern(),
                    callExpr,
                    macroDefinition.name(),
                    macroDefinition.literals());
            if (bindings == null) {
                continue;
            }

            MacroExpansionState state = new MacroExpansionState();
            Expr expanded = expandMacroTemplate(
                    rule.template(),
                    macroDefinition,
                    bindings,
                    Map.of(),
                    state,
                    null);
            return new MacroExpansion(expanded, List.copyOf(state.captureBindings));
        }

        throw new EvalError("no matching syntax-rules pattern for " + macroDefinition.name());
    }

    private Expr expandMacroTemplate(Expr template, MacroDefinition macroDefinition,
                                     MacroBindings bindings, Map<String, String> renameEnv,
                                     MacroExpansionState state, Integer repeatIndex)
            throws EvalError {
        if (template instanceof NumberExpr || template instanceof BoolExpr
                || template instanceof StringExpr || template instanceof CharExpr) {
            return template;
        }
        if (template instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            if (name.equals("...")) {
                throw new EvalError("invalid syntax-rules template");
            }

            String renamed = renameEnv.get(name);
            if (renamed != null) {
                return new SymbolExpr(renamed, symbolExpr.pos());
            }

            Expr bound = bindings.lookup(name, repeatIndex);
            if (bound != null) {
                return bound;
            }

            if (isSpecialForm(name) || macroDefinition.definitionMacros().contains(name)) {
                return template;
            }

            String alias = state.captureAliases.get(name);
            if (alias != null) {
                return new SymbolExpr(alias, symbolExpr.pos());
            }

            try {
                Value captured = macroDefinition.definitionEnv().lookup(name);
                alias = freshHygienicName(name);
                state.captureAliases.put(name, alias);
                state.captureBindings.add(new CaptureBinding(alias, captured));
                return new SymbolExpr(alias, symbolExpr.pos());
            } catch (EvalError ignored) {
                return template;
            }
        }

        if (template instanceof VectorExpr vectorExpr) {
            return new VectorExpr(
                    expandTemplateElements(
                            vectorExpr.elements(),
                            macroDefinition,
                            bindings,
                            renameEnv,
                            state,
                            repeatIndex),
                    vectorExpr.pos());
        }

        ListExpr listExpr = (ListExpr) template;
        Expr letExpansion = expandLetTemplate(
                listExpr.elements(),
                listExpr.pos(),
                macroDefinition,
                bindings,
                renameEnv,
                state,
                repeatIndex);
        if (letExpansion != null) {
            return letExpansion;
        }

        return new ListExpr(
                expandTemplateElements(
                        listExpr.elements(),
                        macroDefinition,
                        bindings,
                        renameEnv,
                        state,
                        repeatIndex),
                listExpr.pos());
    }

    private List<Expr> expandTemplateElements(List<Expr> elements, MacroDefinition macroDefinition,
                                              MacroBindings bindings, Map<String, String> renameEnv,
                                              MacroExpansionState state, Integer repeatIndex)
            throws EvalError {
        List<Expr> expandedElements = new ArrayList<>();
        for (int index = 0; index < elements.size(); ) {
            if (index + 1 < elements.size() && isEllipsisExpr(elements.get(index + 1))) {
                int repeatCount = repetitionCountForTemplate(elements.get(index), bindings);
                for (int nestedIndex = 0; nestedIndex < repeatCount; nestedIndex++) {
                    expandedElements.add(expandMacroTemplate(
                            elements.get(index),
                            macroDefinition,
                            bindings,
                            renameEnv,
                            state,
                            nestedIndex));
                }
                index += 2;
                continue;
            }

            expandedElements.add(expandMacroTemplate(
                    elements.get(index),
                    macroDefinition,
                    bindings,
                    renameEnv,
                            state,
                            repeatIndex));
            index++;
        }
        return expandedElements;
    }

    private Expr expandLetTemplate(List<Expr> elements, SourcePos pos, MacroDefinition macroDefinition,
                                   MacroBindings bindings, Map<String, String> renameEnv,
                                   MacroExpansionState state, Integer repeatIndex)
            throws EvalError {
        if (elements.isEmpty() || !(elements.get(0) instanceof SymbolExpr keywordExpr)
                || !keywordExpr.name().equals("let")) {
            return null;
        }
        if (elements.size() < 3) {
            throw new EvalError("invalid syntax-rules template");
        }
        if (!(elements.get(1) instanceof ListExpr bindingExprs)) {
            return null;
        }

        Map<String, String> bodyRenames = new HashMap<>(renameEnv);
        List<Expr> expandedBindings = new ArrayList<>(bindingExprs.elements().size());
        for (Expr bindingExpr : bindingExprs.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingPartsExpr)) {
                throw new EvalError("invalid syntax-rules template");
            }
            List<Expr> bindingParts = bindingPartsExpr.elements();
            if (bindingParts.size() != 2 || !(bindingParts.get(0) instanceof SymbolExpr bindingNameExpr)) {
                throw new EvalError("invalid syntax-rules template");
            }

            Expr expandedName = bindings.lookup(bindingNameExpr.name(), repeatIndex);
            if (expandedName != null) {
                if (!(expandedName instanceof SymbolExpr)) {
                    throw new EvalError("invalid syntax-rules template");
                }
            } else {
                String renamed = freshHygienicName(bindingNameExpr.name());
                bodyRenames.put(bindingNameExpr.name(), renamed);
                expandedName = new SymbolExpr(renamed, bindingNameExpr.pos());
            }

            Expr expandedValue = expandMacroTemplate(
                    bindingParts.get(1),
                    macroDefinition,
                    bindings,
                    renameEnv,
                    state,
                    repeatIndex);
            expandedBindings.add(new ListExpr(List.of(expandedName, expandedValue), bindingPartsExpr.pos()));
        }

        List<Expr> expandedElements = new ArrayList<>(elements.size());
        expandedElements.add(elements.get(0));
        expandedElements.add(new ListExpr(expandedBindings, bindingExprs.pos()));
        for (int i = 2; i < elements.size(); i++) {
            expandedElements.add(expandMacroTemplate(
                    elements.get(i),
                    macroDefinition,
                    bindings,
                    bodyRenames,
                    state,
                    repeatIndex));
        }
        return new ListExpr(expandedElements, pos);
    }

    private String freshHygienicName(String base) {
        String name = "__macro_" + base + "_" + nextHygieneId;
        nextHygieneId++;
        return name;
    }

    private Value evalLambda(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("invalid lambda");
        }
        ParameterSpec parameters = parseParameters(arguments.get(0));
        return new LambdaProcedure(null, parameters, copyExprs(arguments.subList(1, arguments.size())), env);
    }

    private Value evalCaseLambda(List<Expr> arguments, Environment env) throws EvalError {
        List<CaseLambdaClause> clauses = new ArrayList<>(arguments.size());
        for (Expr clauseExpr : arguments) {
            if (!(clauseExpr instanceof ListExpr clauseList) || clauseList.elements().size() < 2) {
                throw new EvalError("invalid case-lambda");
            }
            List<Expr> clause = clauseList.elements();
            clauses.add(new CaseLambdaClause(
                    parseParameters(clause.get(0)),
                    copyExprs(clause.subList(1, clause.size()))));
        }
        return new CaseLambdaProcedure(null, List.copyOf(clauses), env);
    }

    private Value evalDo(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("invalid do");
        }

        List<DoBinding> bindings = parseDoBindings(arguments.get(0));
        if (!(arguments.get(1) instanceof ListExpr testExpr) || testExpr.elements().isEmpty()) {
            throw new EvalError("invalid do");
        }

        List<Value> initialValues = new ArrayList<>(bindings.size());
        for (DoBinding binding : bindings) {
            initialValues.add(eval(binding.initExpr(), env));
        }

        Environment loopEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            loopEnv.define(bindings.get(i).name(), initialValues.get(i));
        }

        List<Expr> terminationClause = testExpr.elements();
        List<Expr> body = arguments.subList(2, arguments.size());
        while (true) {
            if (isTruthy(eval(terminationClause.get(0), loopEnv))) {
                return terminationClause.size() == 1
                        ? VOID_VALUE
                        : evalSequence(terminationClause.subList(1, terminationClause.size()), loopEnv, tailPosition);
            }

            evalSequence(body, loopEnv);

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

    private Value evalLet(List<Expr> arguments, Environment env, boolean tailPosition) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("invalid let");
        }

        if (arguments.get(0) instanceof SymbolExpr nameExpr) {
            String name = nameExpr.name();
            if (arguments.size() < 3) {
                throw new EvalError("invalid let");
            }
            List<Binding> bindings = parseBindings(arguments.get(1), "let");
            List<Value> values = evalBindingValues(bindings, env);
            List<String> parameters = bindingNames(bindings);

            Environment loopEnv = new Environment(env);
            LambdaProcedure procedure = new LambdaProcedure(
                    name,
                    ParameterSpec.fixed(parameters),
                    copyExprs(arguments.subList(2, arguments.size())),
                    loopEnv);
            loopEnv.define(name, procedure);
            List<LocatedValue> locatedValues = new ArrayList<>(values.size());
            for (int i = 0; i < values.size(); i++) {
                locatedValues.add(new LocatedValue(values.get(i), bindings.get(i).valueExpr().pos()));
            }
            return applyLambda(procedure, locatedValues, arguments.get(1).pos(), tailPosition);
        }

        List<Binding> bindings = parseBindings(arguments.get(0), "let");
        List<Value> values = evalBindingValues(bindings, env);
        Environment letEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            letEnv.define(bindings.get(i).name(), values.get(i));
        }
        return evalSequence(arguments.subList(1, arguments.size()), letEnv, tailPosition);
    }

    private Value evalLetrec(List<Expr> arguments, Environment env, boolean sequential, boolean tailPosition)
            throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        if (arguments.size() < 2) {
            throw new EvalError("invalid " + formName);
        }

        List<Binding> bindings = parseBindings(arguments.get(0), formName);
        Environment letrecEnv = new Environment(env);
        for (Binding binding : bindings) {
            letrecEnv.define(binding.name(), UNINITIALIZED_VALUE);
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

        return evalSequence(arguments.subList(1, arguments.size()), letrecEnv, tailPosition);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        if (arguments.size() != 1) {
            throw new EvalError("wrong argument count for quote");
        }
        return quote(arguments.get(0));
    }

    private Value evalSet(List<Expr> arguments, Environment env) throws EvalError {
        if (arguments.size() != 2 || !(arguments.get(0) instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("invalid set!");
        }

        env.set(symbolExpr.name(), eval(arguments.get(1), env));
        return VOID_VALUE;
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        return evalSequence(expressions, env, false);
    }

    private Value evalSequence(List<Expr> expressions, Environment env, boolean tailPosition) throws EvalError {
        Value result = VOID_VALUE;
        for (int i = 0; i < expressions.size(); i++) {
            result = eval(expressions.get(i), env, tailPosition && i == expressions.size() - 1);
        }
        return result;
    }

    private List<Expr> copyExprs(List<Expr> expressions) {
        return new ArrayList<>(expressions);
    }

    private List<Binding> parseBindings(Expr bindingsExpr, String formName) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("invalid " + formName);
        }
        List<Expr> bindings = bindingsList.elements();

        List<Binding> parsed = new ArrayList<>(bindings.size());
        for (Expr bindingExpr : bindings) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("invalid " + formName);
            }
            List<Expr> binding = bindingList.elements();
            if (binding.size() != 2 || !(binding.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("invalid " + formName);
            }
            parsed.add(new Binding(nameExpr.name(), binding.get(1)));
        }
        return parsed;
    }

    private List<DoBinding> parseDoBindings(Expr bindingsExpr) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("invalid do");
        }

        List<DoBinding> parsed = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("invalid do");
            }
            List<Expr> binding = bindingList.elements();
            if (binding.size() < 2 || binding.size() > 3 || !(binding.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("invalid do");
            }
            parsed.add(new DoBinding(
                    nameExpr.name(),
                    binding.get(1),
                    binding.size() == 3 ? binding.get(2) : null));
        }
        return parsed;
    }

    private List<Value> evalBindingValues(List<Binding> bindings, Environment env) throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(eval(binding.valueExpr(), env));
        }
        return values;
    }

    private List<String> bindingNames(List<Binding> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            names.add(binding.name());
        }
        return names;
    }

    private ParameterSpec parseParameters(Expr parametersExpr) throws EvalError {
        if (parametersExpr instanceof SymbolExpr symbolExpr) {
            if (".".equals(symbolExpr.name())) {
                throw new EvalError("invalid parameter list");
            }
            return ParameterSpec.restOnly(symbolExpr.name());
        }
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw new EvalError("invalid parameter list");
        }
        return parseParameters(parametersList.elements());
    }

    private ParameterSpec parseParameters(List<Expr> parameters) throws EvalError {
        List<String> names = new ArrayList<>(parameters.size());
        for (int i = 0; i < parameters.size(); i++) {
            Expr parameter = parameters.get(i);
            if (!(parameter instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("invalid parameter list");
            }
            if (".".equals(symbolExpr.name())) {
                if (i + 2 != parameters.size()) {
                    throw new EvalError("invalid parameter list");
                }
                Expr restExpr = parameters.get(i + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol) || ".".equals(restSymbol.name())) {
                    throw new EvalError("invalid parameter list");
                }
                return new ParameterSpec(List.copyOf(names), restSymbol.name());
            }
            names.add(symbolExpr.name());
        }
        return ParameterSpec.fixed(names);
    }

    private RecordConstructorSpec parseRecordConstructorSpec(Expr expr) throws EvalError {
        if (!(expr instanceof ListExpr constructorExpr) || constructorExpr.elements().isEmpty()) {
            throw errorAt(expr.pos(), "invalid define-record-type");
        }

        List<Expr> elements = constructorExpr.elements();
        String constructorName = requireSymbolExpr(elements.get(0), "invalid define-record-type");
        List<String> fields = new ArrayList<>(Math.max(0, elements.size() - 1));
        for (int i = 1; i < elements.size(); i++) {
            fields.add(requireSymbolExpr(elements.get(i), "invalid define-record-type"));
        }
        return new RecordConstructorSpec(constructorName, List.copyOf(fields));
    }

    private RecordAccessorSpec parseRecordAccessorSpec(Expr expr) throws EvalError {
        if (!(expr instanceof ListExpr accessorExpr) || accessorExpr.elements().size() != 2) {
            throw errorAt(expr.pos(), "invalid define-record-type");
        }

        List<Expr> elements = accessorExpr.elements();
        return new RecordAccessorSpec(
                requireSymbolExpr(elements.get(0), "invalid define-record-type"),
                requireSymbolExpr(elements.get(1), "invalid define-record-type"));
    }

    private String requireSymbolExpr(Expr expr, String message) throws EvalError {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        throw errorAt(expr.pos(), message);
    }

    private Value quote(Expr expression) {
        if (expression instanceof NumberExpr numberExpr) {
            return numberExpr.value();
        }
        if (expression instanceof BoolExpr boolExpr) {
            return boolValue(boolExpr.value());
        }
        if (expression instanceof StringExpr stringExpr) {
            return new StringValue(stringExpr.value());
        }
        if (expression instanceof CharExpr charExpr) {
            return new CharValue(charExpr.value());
        }
        if (expression instanceof VectorExpr vectorExpr) {
            return quoteVector(vectorExpr.elements());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            return new SymbolValue(symbolExpr.name());
        }
        if (expression instanceof ListExpr listExpr) {
            return quoteList(listExpr.elements());
        }
        throw new IllegalStateException("unknown quoted expression type");
    }

    private Value quoteList(List<Expr> elements) {
        Value value = EMPTY_LIST;
        for (int i = elements.size() - 1; i >= 0; i--) {
            value = new PairValue(quote(elements.get(i)), value);
        }
        return value;
    }

    private Value quoteVector(List<Expr> elements) {
        List<Value> values = new ArrayList<>(elements.size());
        for (Expr element : elements) {
            values.add(quote(element));
        }
        return new VectorValue(values);
    }

    private Value apply(Value operator, SourcePos operatorPos, List<LocatedValue> arguments,
                        SourcePos callPos) throws EvalError {
        return apply(operator, operatorPos, arguments, callPos, false);
    }

    private Value apply(Value operator, SourcePos operatorPos, List<LocatedValue> arguments,
                        SourcePos callPos, boolean tailPosition) throws EvalError {
        if (operator instanceof BuiltinProcedure builtin) {
            return builtin.apply(callPos, arguments);
        }
        if (operator instanceof CaseLambdaProcedure caseLambda) {
            return applyCaseLambda(caseLambda, arguments, callPos, tailPosition);
        }
        if (operator instanceof LambdaProcedure lambda) {
            return applyLambda(lambda, arguments, callPos, tailPosition);
        }
        throw errorAt(operatorPos, "attempted to call non-procedure");
    }

    private Value applyLambda(LambdaProcedure lambda, List<LocatedValue> arguments, SourcePos callPos)
            throws EvalError {
        return applyLambda(lambda, arguments, callPos, false);
    }

    private Value applyLambda(LambdaProcedure lambda, List<LocatedValue> arguments, SourcePos callPos,
                              boolean tailPosition) throws EvalError {
        UserCallTarget target = resolveLambdaCall(lambda, arguments, callPos);
        if (tailPosition) {
            throw new TailCallSignal(target, arguments);
        }
        return applyUserProcedure(target, arguments);
    }

    private UserCallTarget resolveLambdaCall(LambdaProcedure lambda, List<LocatedValue> arguments, SourcePos callPos)
            throws EvalError {
        ParameterSpec parameters = lambda.parameters();
        if (!parameters.accepts(arguments.size())) {
            throw errorAt(callPos, "wrong argument count for " + lambda.displayName());
        }
        return new UserCallTarget(lambda.closure(), parameters, lambda.body());
    }

    private Value applyCaseLambda(CaseLambdaProcedure caseLambda, List<LocatedValue> arguments,
                                  SourcePos callPos) throws EvalError {
        return applyCaseLambda(caseLambda, arguments, callPos, false);
    }

    private Value applyCaseLambda(CaseLambdaProcedure caseLambda, List<LocatedValue> arguments,
                                  SourcePos callPos, boolean tailPosition) throws EvalError {
        UserCallTarget target = resolveCaseLambdaCall(caseLambda, arguments, callPos);
        if (tailPosition) {
            throw new TailCallSignal(target, arguments);
        }
        return applyUserProcedure(target, arguments);
    }

    private UserCallTarget resolveCaseLambdaCall(CaseLambdaProcedure caseLambda, List<LocatedValue> arguments,
                                                 SourcePos callPos) throws EvalError {
        for (CaseLambdaClause clause : caseLambda.clauses()) {
            if (clause.parameters().accepts(arguments.size())) {
                return new UserCallTarget(caseLambda.closure(), clause.parameters(), clause.body());
            }
        }
        throw errorAt(callPos, "wrong argument count for " + caseLambda.displayName());
    }

    private Value applyUserProcedure(UserCallTarget initialTarget, List<LocatedValue> initialArguments)
            throws EvalError {
        UserCallTarget target = initialTarget;
        List<LocatedValue> arguments = initialArguments;

        while (true) {
            ParameterSpec parameters = target.parameters();
            Environment callEnv = new Environment(target.closure());
            for (int i = 0; i < parameters.required().size(); i++) {
                callEnv.define(parameters.required().get(i), arguments.get(i).value());
            }
            if (parameters.rest() != null) {
                callEnv.define(parameters.rest(), buildList(arguments, parameters.required().size()));
            }

            try {
                return evalSequence(target.body(), callEnv, true);
            } catch (TailCallSignal signal) {
                target = signal.target();
                arguments = signal.arguments();
            }
        }
    }

    private Value builtinNot(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "not", callPos);
        return boolValue(!isTruthy(arguments.get(0).value()));
    }

    private Value builtinCons(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "cons", callPos);
        return new PairValue(arguments.get(0).value(), arguments.get(1).value());
    }

    private Value builtinCar(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "car", callPos);
        return requirePair(arguments.get(0), "car").car();
    }

    private Value builtinCdr(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "cdr", callPos);
        return requirePair(arguments.get(0), "cdr").cdr();
    }

    private Value builtinNull(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "null?", callPos);
        return boolValue(arguments.get(0).value() instanceof EmptyListValue);
    }

    private Value builtinList(SourcePos callPos, List<LocatedValue> arguments) {
        Value result = EMPTY_LIST;
        for (int i = arguments.size() - 1; i >= 0; i--) {
            result = new PairValue(arguments.get(i).value(), result);
        }
        return result;
    }

    private Value builtinVector(SourcePos callPos, List<LocatedValue> arguments) {
        List<Value> values = new ArrayList<>(arguments.size());
        for (LocatedValue argument : arguments) {
            values.add(argument.value());
        }
        return new VectorValue(values);
    }

    private Value builtinMakeVector(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.size() < 1 || arguments.size() > 2) {
            throw errorAt(callPos, "wrong argument count for make-vector");
        }

        int length = requireNonNegativeIndex(arguments.get(0), "make-vector");
        Value fill = arguments.size() == 2 ? arguments.get(1).value() : VOID_VALUE;
        List<Value> values = new ArrayList<>(length);
        for (int i = 0; i < length; i++) {
            values.add(fill);
        }
        return new VectorValue(values);
    }

    private Value builtinVectorRef(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "vector-ref", callPos);
        VectorValue vector = requireVector(arguments.get(0), "vector-ref");
        return vector.element(requireVectorIndex(arguments.get(1), "vector-ref", vector.length()));
    }

    private Value builtinVectorSet(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 3, "vector-set!", callPos);
        VectorValue vector = requireVector(arguments.get(0), "vector-set!");
        vector.setElement(
                requireVectorIndex(arguments.get(1), "vector-set!", vector.length()),
                arguments.get(2).value());
        return VOID_VALUE;
    }

    private Value builtinVectorLength(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "vector-length", callPos);
        return new IntValue(requireVector(arguments.get(0), "vector-length").length());
    }

    private Value builtinVectorPredicate(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "vector?", callPos);
        return boolValue(arguments.get(0).value() instanceof VectorValue);
    }

    private Value builtinVectorToList(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "vector->list", callPos);
        return buildListFromValues(requireVector(arguments.get(0), "vector->list").elements());
    }

    private Value builtinListToVector(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "list->vector", callPos);
        return new VectorValue(requireProperListElements(arguments.get(0), "list->vector"));
    }

    private Value builtinLength(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "length", callPos);
        return new IntValue(requireProperListLength(arguments.get(0), "length"));
    }

    private Value builtinAppend(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1).value();
        for (int i = arguments.size() - 2; i >= 0; i--) {
            result = appendListOnto(arguments.get(i), result);
        }
        return result;
    }

    private Value builtinDisplay(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "display", callPos);
        appendOutput(renderValue(arguments.get(0).value(), true));
        return VOID_VALUE;
    }

    private Value builtinWrite(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "write", callPos);
        appendOutput(arguments.get(0).value().render());
        return VOID_VALUE;
    }

    private Value builtinNewline(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 0, "newline", callPos);
        appendOutput("\n");
        return VOID_VALUE;
    }

    private Value builtinStringPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string?", callPos);
        return boolValue(arguments.get(0).value() instanceof StringValue);
    }

    private Value builtinStringAppend(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (LocatedValue argument : arguments) {
            builder.append(requireString(argument, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private Value builtinStringLength(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string-length", callPos);
        return new IntValue(requireString(arguments.get(0), "string-length").length());
    }

    private Value builtinSubstring(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 3, "substring", callPos);
        String value = requireString(arguments.get(0), "substring");
        int start = requireStringIndex(arguments.get(1), "substring", value.length());
        int end = requireStringIndex(arguments.get(2), "substring", value.length());
        if (start > end) {
            throw errorAt(arguments.get(1).pos(), "invalid substring range");
        }
        return new StringValue(value.substring(start, end));
    }

    private Value builtinStringCopy(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string-copy", callPos);
        return requireStringValue(arguments.get(0), "string-copy").copy();
    }

    private Value builtinStringToList(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string->list", callPos);
        String value = requireString(arguments.get(0), "string->list");
        List<Value> chars = new ArrayList<>(value.length());
        for (int i = 0; i < value.length(); i++) {
            chars.add(new CharValue(value.charAt(i)));
        }
        return buildListFromValues(chars);
    }

    private Value builtinListToString(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "list->string", callPos);
        List<Value> elements = requireProperListElements(arguments.get(0), "list->string");
        StringBuilder builder = new StringBuilder(elements.size());
        for (Value element : elements) {
            if (element instanceof CharValue(char ch)) {
                builder.append(ch);
            } else {
                throw errorAt(arguments.get(0).pos(), "expected char for list->string");
            }
        }
        return new StringValue(builder.toString());
    }

    private Value builtinStringSet(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 3, "string-set!", callPos);
        StringValue string = requireStringValue(arguments.get(0), "string-set!");
        if (!string.isMutable()) {
            throw errorAt(callPos, "string-set! not supported on immutable strings");
        }
        int index = requireStringIndex(arguments.get(1), "string-set!", string.length() - 1);
        string.setCharAt(index, requireChar(arguments.get(2), "string-set!"));
        return VOID_VALUE;
    }

    private Value builtinStringToNumber(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string->number", callPos);
        String value = requireString(arguments.get(0), "string->number");
        try {
            NumberValue parsed = parseNumberLiteral(value);
            return parsed == null ? FALSE_VALUE : parsed;
        } catch (IllegalArgumentException e) {
            return FALSE_VALUE;
        }
    }

    private Value builtinNumberToString(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "number->string", callPos);
        return new StringValue(requireNumber(arguments.get(0), "number->string").render());
    }

    private Value builtinSymbolToString(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "symbol->string", callPos);
        return new StringValue(requireSymbol(arguments.get(0), "symbol->string"));
    }

    private Value builtinStringToSymbol(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string->symbol", callPos);
        return new SymbolValue(requireString(arguments.get(0), "string->symbol"));
    }

    private Value builtinStringRef(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "string-ref", callPos);
        String value = requireString(arguments.get(0), "string-ref");
        int index = requireStringIndex(arguments.get(1), "string-ref", value.length() - 1);
        return new CharValue(value.charAt(index));
    }

    private Value builtinCharToInteger(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char->integer", callPos);
        return new IntValue(requireChar(arguments.get(0), "char->integer"));
    }

    private Value builtinIntegerToChar(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "integer->char", callPos);
        long value = requireInt(arguments.get(0), "integer->char");
        if (value < Character.MIN_VALUE || value > Character.MAX_VALUE) {
            throw errorAt(arguments.get(0).pos(), "integer out of range for integer->char");
        }
        return new CharValue((char) value);
    }

    private Value builtinNumberPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "number?", callPos);
        return boolValue(arguments.get(0).value() instanceof NumberValue);
    }

    private Value builtinIntegerPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "integer?", callPos);
        Value value = arguments.get(0).value();
        if (value instanceof IntValue) {
            return TRUE_VALUE;
        }
        if (value instanceof InexactValue(double number)) {
            return boolValue(Double.isFinite(number) && Math.rint(number) == number);
        }
        return FALSE_VALUE;
    }

    private Value builtinRationalPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "rational?", callPos);
        return boolValue(arguments.get(0).value() instanceof NumberValue);
    }

    private Value builtinExactPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "exact?", callPos);
        Value value = arguments.get(0).value();
        return boolValue(value instanceof IntValue || value instanceof RationalValue);
    }

    private Value builtinInexactPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "inexact?", callPos);
        return boolValue(arguments.get(0).value() instanceof InexactValue);
    }

    private Value builtinExactToInexact(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "exact->inexact", callPos);
        NumberValue number = requireNumber(arguments.get(0), "exact->inexact");
        if (number instanceof InexactValue) {
            return number;
        }
        return new InexactValue(numberToDouble(number));
    }

    private Value builtinInexactToExact(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "inexact->exact", callPos);
        NumberValue number = requireNumber(arguments.get(0), "inexact->exact");
        if (number instanceof InexactValue inexact) {
            return inexactToExactValue(inexact.value());
        }
        return number;
    }

    private Value builtinNumerator(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "numerator", callPos);
        NumberValue number = requireNumber(arguments.get(0), "numerator");
        if (number instanceof InexactValue) {
            throw errorAt(arguments.get(0).pos(), "expected exact number for numerator");
        }
        ExactNumber exact = toExactNumber(number);
        return new IntValue(exact.numerator().longValueExact());
    }

    private Value builtinDenominator(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "denominator", callPos);
        NumberValue number = requireNumber(arguments.get(0), "denominator");
        if (number instanceof InexactValue) {
            throw errorAt(arguments.get(0).pos(), "expected exact number for denominator");
        }
        ExactNumber exact = toExactNumber(number);
        return new IntValue(exact.denominator().longValueExact());
    }

    private Value builtinBooleanPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "boolean?", callPos);
        return boolValue(arguments.get(0).value() instanceof BoolValue);
    }

    private Value builtinCharPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char?", callPos);
        return boolValue(arguments.get(0).value() instanceof CharValue);
    }

    private Value builtinPairPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "pair?", callPos);
        return boolValue(arguments.get(0).value() instanceof PairValue);
    }

    private Value builtinProcedurePredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "procedure?", callPos);
        return boolValue(isProcedureValue(arguments.get(0).value()));
    }

    private Value builtinSymbolPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "symbol?", callPos);
        return boolValue(arguments.get(0).value() instanceof SymbolValue);
    }

    private Value builtinEq(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "eq?", callPos);
        return boolValue(eqValues(arguments.get(0).value(), arguments.get(1).value()));
    }

    private Value builtinEqv(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "eqv?", callPos);
        return boolValue(eqvValues(arguments.get(0).value(), arguments.get(1).value()));
    }

    private Value builtinEqual(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "equal?", callPos);
        return boolValue(equalValues(arguments.get(0).value(), arguments.get(1).value()));
    }

    private Value builtinApply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for apply");
        }

        LocatedValue operator = arguments.get(0);
        List<LocatedValue> expandedArguments = new ArrayList<>();
        for (int i = 1; i < arguments.size() - 1; i++) {
            expandedArguments.add(arguments.get(i));
        }
        expandedArguments.addAll(expandApplyArguments(arguments.get(arguments.size() - 1)));
        return apply(operator.value(), operator.pos(), expandedArguments, callPos);
    }

    private Value builtinMap(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for map");
        }

        LocatedValue operator = arguments.get(0);
        List<LocatedValue> listArguments = arguments.subList(1, arguments.size());
        List<Value> cursors = new ArrayList<>(listArguments.size());
        for (LocatedValue listArgument : listArguments) {
            cursors.add(listArgument.value());
        }

        List<Value> results = new ArrayList<>();
        while (true) {
            int emptyCount = 0;
            for (int i = 0; i < cursors.size(); i++) {
                Value current = cursors.get(i);
                if (current instanceof EmptyListValue) {
                    emptyCount++;
                    continue;
                }
                if (!(current instanceof PairValue)) {
                    throw errorAt(listArguments.get(i).pos(), "expected list for map");
                }
            }

            if (emptyCount > 0) {
                if (emptyCount != cursors.size()) {
                    throw errorAt(callPos, "expected lists of equal length for map");
                }
                return buildListFromValues(results);
            }

            List<LocatedValue> mappedArguments = new ArrayList<>(cursors.size());
            for (int i = 0; i < cursors.size(); i++) {
                PairValue pair = (PairValue) cursors.get(i);
                mappedArguments.add(new LocatedValue(pair.car(), listArguments.get(i).pos()));
                cursors.set(i, pair.cdr());
            }
            results.add(apply(operator.value(), operator.pos(), mappedArguments, callPos));
        }
    }

    private Value builtinAdd(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        NumberValue total = new IntValue(0L);
        for (LocatedValue argument : arguments) {
            total = addNumbers(total, requireNumber(argument, "+"));
        }
        return total;
    }

    private Value builtinAbs(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 1, "abs", callPos);
        NumberValue number = requireNumber(arguments.get(0), "abs");
        if (number instanceof InexactValue(double value)) {
            return new InexactValue(Math.abs(value));
        }
        ExactNumber exact = toExactNumber(number);
        return exactValue(exact.numerator().abs(), exact.denominator());
    }

    private Value builtinSubtract(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw errorAt(callPos, "wrong argument count for -");
        }

        NumberValue result = requireNumber(arguments.get(0), "-");
        if (arguments.size() == 1) {
            return subtractNumbers(new IntValue(0L), result);
        }

        for (int i = 1; i < arguments.size(); i++) {
            result = subtractNumbers(result, requireNumber(arguments.get(i), "-"));
        }
        return result;
    }

    private Value builtinModulo(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "modulo", callPos);
        long dividend = requireInt(arguments.get(0), "modulo");
        long divisor = requireNonZeroDivisor(arguments.get(1), "modulo");
        long remainder = dividend % divisor;
        if (remainder != 0 && ((remainder < 0) != (divisor < 0))) {
            remainder += divisor;
        }
        return new IntValue(remainder);
    }

    private Value builtinRemainder(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "remainder", callPos);
        long dividend = requireInt(arguments.get(0), "remainder");
        long divisor = requireNonZeroDivisor(arguments.get(1), "remainder");
        return new IntValue(dividend % divisor);
    }

    private Value builtinQuotient(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "quotient", callPos);
        long dividend = requireInt(arguments.get(0), "quotient");
        long divisor = requireNonZeroDivisor(arguments.get(1), "quotient");
        return new IntValue(dividend / divisor);
    }

    private Value builtinMin(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw errorAt(callPos, "wrong argument count for min");
        }

        NumberValue result = requireNumber(arguments.get(0), "min");
        for (int i = 1; i < arguments.size(); i++) {
            NumberValue next = requireNumber(arguments.get(i), "min");
            if (compareNumbers(next, result) < 0) {
                result = next;
            }
        }
        return result;
    }

    private Value builtinMax(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            throw errorAt(callPos, "wrong argument count for max");
        }

        NumberValue result = requireNumber(arguments.get(0), "max");
        for (int i = 1; i < arguments.size(); i++) {
            NumberValue next = requireNumber(arguments.get(i), "max");
            if (compareNumbers(next, result) > 0) {
                result = next;
            }
        }
        return result;
    }

    private Value builtinExpt(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "expt", callPos);
        long base = requireInt(arguments.get(0), "expt");
        long exponent = requireInt(arguments.get(1), "expt");
        if (exponent < 0) {
            throw errorAt(arguments.get(1).pos(), "expected non-negative integer for expt");
        }

        long result = 1L;
        long factor = base;
        long power = exponent;
        while (power > 0) {
            if ((power & 1L) != 0L) {
                result *= factor;
            }
            factor *= factor;
            power >>= 1;
        }
        return new IntValue(result);
    }

    private Value builtinZeroPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "zero?", callPos);
        return boolValue(isZero(requireNumber(arguments.get(0), "zero?")));
    }

    private Value builtinPositivePredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "positive?", callPos);
        return boolValue(compareNumbers(requireNumber(arguments.get(0), "positive?"), new IntValue(0L)) > 0);
    }

    private Value builtinNegativePredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "negative?", callPos);
        return boolValue(compareNumbers(requireNumber(arguments.get(0), "negative?"), new IntValue(0L)) < 0);
    }

    private Value builtinOddPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "odd?", callPos);
        return boolValue((requireInt(arguments.get(0), "odd?") & 1L) != 0L);
    }

    private Value builtinEvenPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "even?", callPos);
        return boolValue((requireInt(arguments.get(0), "even?") & 1L) == 0L);
    }

    private Value builtinMultiply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        NumberValue total = new IntValue(1L);
        for (LocatedValue argument : arguments) {
            total = multiplyNumbers(total, requireNumber(argument, "*"));
        }
        return total;
    }

    private Value builtinDivide(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for /");
        }

        NumberValue result = requireNumber(arguments.get(0), "/");
        for (int i = 1; i < arguments.size(); i++) {
            NumberValue divisor = requireNumber(arguments.get(i), "/");
            if (isZero(divisor)) {
                throw errorAt(arguments.get(i).pos(), "division by zero");
            }
            result = divideNumbers(result, divisor);
        }
        return result;
    }

    private Value builtinComparison(SourcePos callPos, List<LocatedValue> arguments,
                                    Comparison comparison) throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for " + comparison.name);
        }

        NumberValue left = requireNumber(arguments.get(0), comparison.name);
        for (int i = 1; i < arguments.size(); i++) {
            NumberValue right = requireNumber(arguments.get(i), comparison.name);
            if (!comparison.test(compareNumbers(left, right))) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinListPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "list?", callPos);
        return boolValue(isProperList(arguments.get(0).value()));
    }

    private Value builtinListRef(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "list-ref", callPos);
        int index = requireNonNegativeIndex(arguments.get(1), "list-ref");
        Value current = arguments.get(0).value();
        for (int i = 0; ; i++) {
            if (current instanceof PairValue(Value car, Value cdr)) {
                if (i == index) {
                    return car;
                }
                current = cdr;
                continue;
            }
            if (current instanceof EmptyListValue) {
                throw errorAt(arguments.get(1).pos(), "index out of range for list-ref");
            }
            throw errorAt(arguments.get(0).pos(), "expected list for list-ref");
        }
    }

    private Value builtinListTail(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "list-tail", callPos);
        int index = requireNonNegativeIndex(arguments.get(1), "list-tail");
        Value current = arguments.get(0).value();
        for (int i = 0; i < index; i++) {
            if (current instanceof PairValue(Value ignoredCar, Value cdr)) {
                current = cdr;
                continue;
            }
            if (current instanceof EmptyListValue) {
                throw errorAt(arguments.get(1).pos(), "index out of range for list-tail");
            }
            throw errorAt(arguments.get(0).pos(), "expected list for list-tail");
        }
        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw errorAt(arguments.get(0).pos(), "expected list for list-tail");
    }

    private Value builtinAssoc(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
        expectArgumentCount(arguments, 2, "assoc", callPos);
        Value key = arguments.get(0).value();
        Value current = arguments.get(1).value();
        while (current instanceof PairValue(Value entry, Value rest)) {
            if (!(entry instanceof PairValue pair)) {
                throw errorAt(arguments.get(1).pos(), "expected association list for assoc");
            }
            if (equalValues(key, pair.car())) {
                return entry;
            }
            current = rest;
        }
        if (current instanceof EmptyListValue) {
            return FALSE_VALUE;
        }
        throw errorAt(arguments.get(1).pos(), "expected association list for assoc");
    }

    private Value builtinCharAlphabeticPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char-alphabetic?", callPos);
        return boolValue(Character.isLetter(requireChar(arguments.get(0), "char-alphabetic?")));
    }

    private Value builtinCharNumericPredicate(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char-numeric?", callPos);
        return boolValue(Character.isDigit(requireChar(arguments.get(0), "char-numeric?")));
    }

    private Value builtinCharUpcase(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char-upcase", callPos);
        return new CharValue(Character.toUpperCase(requireChar(arguments.get(0), "char-upcase")));
    }

    private Value builtinCharDowncase(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "char-downcase", callPos);
        return new CharValue(Character.toLowerCase(requireChar(arguments.get(0), "char-downcase")));
    }

    private Value builtinCharEquals(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for char=?");
        }

        char left = requireChar(arguments.get(0), "char=?");
        for (int i = 1; i < arguments.size(); i++) {
            char right = requireChar(arguments.get(i), "char=?");
            if (left != right) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinCharLessThan(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for char<?");
        }

        char left = requireChar(arguments.get(0), "char<?");
        for (int i = 1; i < arguments.size(); i++) {
            char right = requireChar(arguments.get(i), "char<?");
            if (left >= right) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinStringEquals(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for string=?");
        }

        String left = requireString(arguments.get(0), "string=?");
        for (int i = 1; i < arguments.size(); i++) {
            String right = requireString(arguments.get(i), "string=?");
            if (!left.equals(right)) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinStringLessThan(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for string<?");
        }

        String left = requireString(arguments.get(0), "string<?");
        for (int i = 1; i < arguments.size(); i++) {
            String right = requireString(arguments.get(i), "string<?");
            if (left.compareTo(right) >= 0) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinStringCaseInsensitiveEquals(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        if (arguments.size() < 2) {
            throw errorAt(callPos, "wrong argument count for string-ci=?");
        }

        String left = requireString(arguments.get(0), "string-ci=?");
        for (int i = 1; i < arguments.size(); i++) {
            String right = requireString(arguments.get(i), "string-ci=?");
            if (!left.equalsIgnoreCase(right)) {
                return FALSE_VALUE;
            }
            left = right;
        }
        return TRUE_VALUE;
    }

    private Value builtinStringUpcase(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string-upcase", callPos);
        return new StringValue(requireString(arguments.get(0), "string-upcase")
                .toUpperCase(Locale.ROOT));
    }

    private Value builtinStringDowncase(SourcePos callPos, List<LocatedValue> arguments)
            throws EvalError {
        expectArgumentCount(arguments, 1, "string-downcase", callPos);
        return new StringValue(requireString(arguments.get(0), "string-downcase")
                .toLowerCase(Locale.ROOT));
    }

    private Value constructRecord(SourcePos callPos, List<LocatedValue> arguments, String name,
                                  RecordType recordType) throws EvalError {
        expectArgumentCount(arguments, recordType.fieldCount(), name, callPos);
        List<Value> fields = new ArrayList<>(arguments.size());
        for (LocatedValue argument : arguments) {
            fields.add(argument.value());
        }
        return new RecordValue(recordType, fields);
    }

    private Value recordPredicate(SourcePos callPos, List<LocatedValue> arguments, String name,
                                  RecordType recordType) throws EvalError {
        expectArgumentCount(arguments, 1, name, callPos);
        return boolValue(arguments.get(0).value() instanceof RecordValue record
                && record.type() == recordType);
    }

    private Value recordAccessor(SourcePos callPos, List<LocatedValue> arguments, String name,
                                 RecordType recordType, int fieldIndex) throws EvalError {
        expectArgumentCount(arguments, 1, name, callPos);
        return requireRecord(arguments.get(0), name, recordType).field(fieldIndex);
    }

    private static NumberValue parseNumberLiteral(String token) {
        if (isIntegerToken(token)) {
            try {
                return exactValue(new BigInteger(token), BigInteger.ONE);
            } catch (RuntimeException e) {
                throw new IllegalArgumentException("invalid integer literal: " + token, e);
            }
        }
        if (isRationalToken(token)) {
            try {
                int slash = token.indexOf('/');
                BigInteger numerator = new BigInteger(token.substring(0, slash));
                BigInteger denominator = new BigInteger(token.substring(slash + 1));
                if (denominator.signum() == 0) {
                    throw new IllegalArgumentException("invalid rational literal: " + token);
                }
                return exactValue(numerator, denominator);
            } catch (RuntimeException e) {
                if (e instanceof IllegalArgumentException illegal && illegal.getMessage() != null
                        && illegal.getMessage().startsWith("invalid rational literal: ")) {
                    throw illegal;
                }
                throw new IllegalArgumentException("invalid rational literal: " + token, e);
            }
        }
        if (isInexactToken(token)) {
            try {
                return new InexactValue(Double.parseDouble(token));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("invalid inexact literal: " + token, e);
            }
        }
        return null;
    }

    private static boolean isIntegerToken(String token) {
        if (token.isEmpty()) {
            return false;
        }

        int start = 0;
        if (token.charAt(0) == '-' || token.charAt(0) == '+') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }

        return isUnsignedIntegerToken(token.substring(start));
    }

    private static boolean isUnsignedIntegerToken(String token) {
        if (token.isEmpty()) {
            return false;
        }
        for (int i = 0; i < token.length(); i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isRationalToken(String token) {
        int slash = token.indexOf('/');
        return slash > 0
                && slash == token.lastIndexOf('/')
                && isIntegerToken(token.substring(0, slash))
                && isUnsignedIntegerToken(token.substring(slash + 1));
    }

    private static boolean isInexactToken(String token) {
        if (token.isEmpty()) {
            return false;
        }

        int start = 0;
        if (token.charAt(0) == '-' || token.charAt(0) == '+') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }

        boolean sawDigit = false;
        boolean sawDot = false;
        for (int i = start; i < token.length(); i++) {
            char ch = token.charAt(i);
            if (Character.isDigit(ch)) {
                sawDigit = true;
                continue;
            }
            if (ch == '.' && !sawDot) {
                sawDot = true;
                continue;
            }
            return false;
        }
        return sawDigit && sawDot;
    }

    private NumberValue requireNumber(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof NumberValue number) {
            return number;
        }
        throw errorAt(value.pos(), "expected number for " + name);
    }

    private static boolean isZero(NumberValue number) {
        if (number instanceof IntValue(long value)) {
            return value == 0L;
        }
        if (number instanceof RationalValue(long numerator, long ignoredDenominator)) {
            return numerator == 0L;
        }
        return ((InexactValue) number).value() == 0.0d;
    }

    private static NumberValue addNumbers(NumberValue left, NumberValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return new InexactValue(numberToDouble(left) + numberToDouble(right));
        }
        ExactNumber leftExact = toExactNumber(left);
        ExactNumber rightExact = toExactNumber(right);
        return exactValue(
                leftExact.numerator().multiply(rightExact.denominator())
                        .add(rightExact.numerator().multiply(leftExact.denominator())),
                leftExact.denominator().multiply(rightExact.denominator()));
    }

    private static NumberValue subtractNumbers(NumberValue left, NumberValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return new InexactValue(numberToDouble(left) - numberToDouble(right));
        }
        ExactNumber leftExact = toExactNumber(left);
        ExactNumber rightExact = toExactNumber(right);
        return exactValue(
                leftExact.numerator().multiply(rightExact.denominator())
                        .subtract(rightExact.numerator().multiply(leftExact.denominator())),
                leftExact.denominator().multiply(rightExact.denominator()));
    }

    private static NumberValue multiplyNumbers(NumberValue left, NumberValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return new InexactValue(numberToDouble(left) * numberToDouble(right));
        }
        ExactNumber leftExact = toExactNumber(left);
        ExactNumber rightExact = toExactNumber(right);
        return exactValue(
                leftExact.numerator().multiply(rightExact.numerator()),
                leftExact.denominator().multiply(rightExact.denominator()));
    }

    private static NumberValue divideNumbers(NumberValue left, NumberValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return new InexactValue(numberToDouble(left) / numberToDouble(right));
        }
        ExactNumber leftExact = toExactNumber(left);
        ExactNumber rightExact = toExactNumber(right);
        return exactValue(
                leftExact.numerator().multiply(rightExact.denominator()),
                leftExact.denominator().multiply(rightExact.numerator()));
    }

    private static int compareNumbers(NumberValue left, NumberValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return Double.compare(numberToDouble(left), numberToDouble(right));
        }
        ExactNumber leftExact = toExactNumber(left);
        ExactNumber rightExact = toExactNumber(right);
        return leftExact.numerator().multiply(rightExact.denominator())
                .compareTo(rightExact.numerator().multiply(leftExact.denominator()));
    }

    private static boolean numberValuesEqual(NumberValue left, NumberValue right) {
        return compareNumbers(left, right) == 0;
    }

    private static boolean sameNumberLiteral(NumberValue left, NumberValue right) {
        if (left instanceof IntValue(long leftInt) && right instanceof IntValue(long rightInt)) {
            return leftInt == rightInt;
        }
        if (left instanceof RationalValue(long leftNumerator, long leftDenominator)
                && right instanceof RationalValue(long rightNumerator, long rightDenominator)) {
            return leftNumerator == rightNumerator && leftDenominator == rightDenominator;
        }
        if (left instanceof InexactValue(double leftInexact)
                && right instanceof InexactValue(double rightInexact)) {
            return Double.doubleToLongBits(leftInexact) == Double.doubleToLongBits(rightInexact);
        }
        return false;
    }

    private static double numberToDouble(NumberValue number) {
        if (number instanceof IntValue(long value)) {
            return (double) value;
        }
        if (number instanceof RationalValue(long numerator, long denominator)) {
            return (double) numerator / (double) denominator;
        }
        return ((InexactValue) number).value();
    }

    private static NumberValue inexactToExactValue(double value) {
        BigDecimal decimal = BigDecimal.valueOf(value).stripTrailingZeros();
        BigInteger numerator = decimal.unscaledValue();
        BigInteger denominator = BigInteger.ONE;
        if (decimal.scale() > 0) {
            denominator = BigInteger.TEN.pow(decimal.scale());
        } else if (decimal.scale() < 0) {
            numerator = numerator.multiply(BigInteger.TEN.pow(-decimal.scale()));
        }
        return exactValue(numerator, denominator);
    }

    private static ExactNumber toExactNumber(NumberValue number) {
        if (number instanceof IntValue(long value)) {
            return new ExactNumber(BigInteger.valueOf(value), BigInteger.ONE);
        }
        if (number instanceof RationalValue(long numerator, long denominator)) {
            return new ExactNumber(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator));
        }
        throw new IllegalArgumentException("expected exact number");
    }

    private static NumberValue exactValue(BigInteger numerator, BigInteger denominator) {
        ExactNumber exact = new ExactNumber(numerator, denominator);
        if (exact.denominator().equals(BigInteger.ONE)) {
            return new IntValue(exact.numerator().longValueExact());
        }
        return new RationalValue(
                exact.numerator().longValueExact(),
                exact.denominator().longValueExact());
    }

    private void expectArgumentCount(List<?> arguments, int expected, String name, SourcePos pos)
            throws EvalError {
        if (arguments.size() != expected) {
            throw errorAt(pos, "wrong argument count for " + name);
        }
    }

    private long requireInt(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof IntValue(long number)) {
            return number;
        }
        throw errorAt(value.pos(), "expected number for " + name);
    }

    private int requireNonNegativeIndex(LocatedValue value, String name) throws EvalError {
        long index = requireInt(value, name);
        if (index < 0 || index > Integer.MAX_VALUE) {
            throw errorAt(value.pos(), "index out of range for " + name);
        }
        return (int) index;
    }

    private long requireNonZeroDivisor(LocatedValue value, String name) throws EvalError {
        long divisor = requireInt(value, name);
        if (divisor == 0L) {
            throw errorAt(value.pos(), "division by zero");
        }
        return divisor;
    }

    private String requireString(LocatedValue value, String name) throws EvalError {
        return requireStringValue(value, name).text();
    }

    private StringValue requireStringValue(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof StringValue stringValue) {
            return stringValue;
        }
        throw errorAt(value.pos(), "expected string for " + name);
    }

    private String requireSymbol(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof SymbolValue(String symbol)) {
            return symbol;
        }
        throw errorAt(value.pos(), "expected symbol for " + name);
    }

    private char requireChar(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof CharValue(char ch)) {
            return ch;
        }
        throw errorAt(value.pos(), "expected char for " + name);
    }

    private int requireStringIndex(LocatedValue value, String name, int upperBound) throws EvalError {
        long index = requireInt(value, name);
        if (index < 0 || index > upperBound) {
            throw errorAt(value.pos(), "index out of range for " + name);
        }
        return (int) index;
    }

    private PairValue requirePair(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof PairValue pair) {
            return pair;
        }
        throw errorAt(value.pos(), "expected pair for " + name);
    }

    private VectorValue requireVector(LocatedValue value, String name) throws EvalError {
        if (value.value() instanceof VectorValue vector) {
            return vector;
        }
        throw errorAt(value.pos(), "expected vector for " + name);
    }

    private RecordValue requireRecord(LocatedValue value, String name, RecordType recordType)
            throws EvalError {
        if (value.value() instanceof RecordValue recordValue && recordValue.type() == recordType) {
            return recordValue;
        }
        throw errorAt(value.pos(), "expected " + recordType.name() + " for " + name);
    }

    private List<Value> requireProperListElements(LocatedValue value, String name) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value.value();
        while (current instanceof PairValue(Value car, Value cdr)) {
            elements.add(car);
            current = cdr;
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw errorAt(value.pos(), "expected list for " + name);
    }

    private long requireProperListLength(LocatedValue value, String name) throws EvalError {
        long length = 0L;
        Value current = value.value();
        while (current instanceof PairValue(Value ignoredCar, Value cdr)) {
            length++;
            current = cdr;
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw errorAt(value.pos(), "expected list for " + name);
    }

    private Value appendListOnto(LocatedValue list, Value tail) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = list.value();
        while (current instanceof PairValue(Value car, Value cdr)) {
            elements.add(car);
            current = cdr;
        }
        if (!(current instanceof EmptyListValue)) {
            throw errorAt(list.pos(), "expected list for append");
        }

        Value result = tail;
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new PairValue(elements.get(i), result);
        }
        return result;
    }

    private Value buildList(List<LocatedValue> values, int startIndex) {
        Value result = EMPTY_LIST;
        for (int i = values.size() - 1; i >= startIndex; i--) {
            result = new PairValue(values.get(i).value(), result);
        }
        return result;
    }

    private Value buildListFromValues(List<Value> values) {
        Value result = EMPTY_LIST;
        for (int i = values.size() - 1; i >= 0; i--) {
            result = new PairValue(values.get(i), result);
        }
        return result;
    }

    private int requireVectorIndex(LocatedValue value, String name, int length) throws EvalError {
        long index = requireInt(value, name);
        if (index < 0 || index >= length) {
            throw errorAt(value.pos(), "index out of range for " + name);
        }
        return (int) index;
    }

    private List<LocatedValue> expandApplyArguments(LocatedValue list) throws EvalError {
        List<LocatedValue> values = new ArrayList<>();
        Value current = list.value();
        while (current instanceof PairValue(Value car, Value cdr)) {
            values.add(new LocatedValue(car, list.pos()));
            current = cdr;
        }
        if (!(current instanceof EmptyListValue)) {
            throw errorAt(list.pos(), "expected list for apply");
        }
        return values;
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue(boolean bool) && !bool);
    }

    private boolean isProperList(Value value) {
        Value current = value;
        while (current instanceof PairValue(Value ignoredCar, Value cdr)) {
            current = cdr;
        }
        return current instanceof EmptyListValue;
    }

    private boolean eqValues(Value left, Value right) {
        return eqvValues(left, right);
    }

    private boolean eqvValues(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof NumberValue leftNumber && right instanceof NumberValue rightNumber) {
            return numberValuesEqual(leftNumber, rightNumber);
        }
        if (left instanceof BoolValue(boolean leftBool) && right instanceof BoolValue(boolean rightBool)) {
            return leftBool == rightBool;
        }
        if (left instanceof CharValue(char leftChar) && right instanceof CharValue(char rightChar)) {
            return leftChar == rightChar;
        }
        if (left instanceof SymbolValue(String leftSymbol) && right instanceof SymbolValue(String rightSymbol)) {
            return leftSymbol.equals(rightSymbol);
        }
        return left instanceof EmptyListValue && right instanceof EmptyListValue;
    }

    private boolean equalValues(Value left, Value right) {
        if (eqvValues(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.text().equals(rightString.text());
        }
        if (left instanceof PairValue(Value leftCar, Value leftCdr)
                && right instanceof PairValue(Value rightCar, Value rightCdr)) {
            return equalValues(leftCar, rightCar) && equalValues(leftCdr, rightCdr);
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            for (int i = 0; i < leftVector.length(); i++) {
                if (!equalValues(leftVector.element(i), rightVector.element(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private BoolValue boolValue(boolean value) {
        return value ? TRUE_VALUE : FALSE_VALUE;
    }

    private void appendOutput(String value) {
        outputBuffer.append(value);
    }

    private EvalError errorAt(SourcePos pos, String message) {
        return new EvalError(message, pos.line(), pos.column());
    }

    private EvalError withPosition(EvalError error, SourcePos pos) {
        return error.hasPosition() ? error : error.withPosition(pos.line(), pos.column());
    }

    private sealed interface Expr permits NumberExpr, BoolExpr, StringExpr, CharExpr, VectorExpr, SymbolExpr, ListExpr {
        SourcePos pos();
    }

    private record SourcePos(int line, int column) {}

    private record NumberExpr(NumberValue value, SourcePos pos) implements Expr {}

    private record BoolExpr(boolean value, SourcePos pos) implements Expr {}

    private record StringExpr(String value, SourcePos pos) implements Expr {}

    private record CharExpr(char value, SourcePos pos) implements Expr {}

    private record VectorExpr(List<Expr> elements, SourcePos pos) implements Expr {}

    private record SymbolExpr(String name, SourcePos pos) implements Expr {}

    private record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {}

    private record LocatedValue(Value value, SourcePos pos) {}

    private record UserCallTarget(Environment closure, ParameterSpec parameters, List<Expr> body) {}

    private static final class TailCallSignal extends RuntimeException {
        private final UserCallTarget target;
        private final List<LocatedValue> arguments;

        private TailCallSignal(UserCallTarget target, List<LocatedValue> arguments) {
            super(null, null, false, false);
            this.target = target;
            this.arguments = arguments;
        }

        private UserCallTarget target() {
            return target;
        }

        private List<LocatedValue> arguments() {
            return arguments;
        }
    }

    private record ExactNumber(BigInteger numerator, BigInteger denominator) {
        private ExactNumber {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("zero denominator");
            }
            if (denominator.signum() < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }
            BigInteger gcd = numerator.gcd(denominator);
            if (gcd.signum() != 0) {
                numerator = numerator.divide(gcd);
                denominator = denominator.divide(gcd);
            }
        }
    }

    private sealed interface Value permits NumberValue, BoolValue, StringValue, SymbolValue,
            CharValue, PairValue, VectorValue, EmptyListValue, BuiltinProcedure, LambdaProcedure,
            CaseLambdaProcedure, RecordValue, UninitializedValue, VoidValue {
        String render();
    }

    private sealed interface NumberValue extends Value permits IntValue, RationalValue, InexactValue {}

    private record IntValue(long value) implements NumberValue {
        @Override
        public String render() {
            return Long.toString(value);
        }
    }

    private record RationalValue(long numerator, long denominator) implements NumberValue {
        @Override
        public String render() {
            return numerator + "/" + denominator;
        }
    }

    private record InexactValue(double value) implements NumberValue {
        @Override
        public String render() {
            return Double.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        @Override
        public String render() {
            return value ? "#t" : "#f";
        }
    }

    private record SymbolValue(String name) implements Value {
        @Override
        public String render() {
            return name;
        }
    }

    private static final class StringValue implements Value {
        private final StringBuilder builder;
        private final boolean mutable;

        private StringValue(String value) {
            this(value, false);
        }

        private StringValue(String value, boolean mutable) {
            this.builder = new StringBuilder(value);
            this.mutable = mutable;
        }

        private String text() {
            return builder.toString();
        }

        private int length() {
            return builder.length();
        }

        private void setCharAt(int index, char value) {
            builder.setCharAt(index, value);
        }

        private boolean isMutable() {
            return mutable;
        }

        private StringValue copy() {
            return new StringValue(text(), true);
        }

        @Override
        public String render() {
            return renderStringLiteral(text());
        }
    }

    private record CharValue(char value) implements Value {
        @Override
        public String render() {
            return renderCharacterLiteral(value);
        }
    }

    private record PairValue(Value car, Value cdr) implements Value {
        @Override
        public String render() {
            return renderPair(this, false);
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

        private Value element(int index) {
            return elements.get(index);
        }

        private void setElement(int index, Value value) {
            elements.set(index, value);
        }

        private List<Value> elements() {
            return List.copyOf(elements);
        }

        @Override
        public String render() {
            return renderVector(this, false);
        }
    }

    private record EmptyListValue() implements Value {
        @Override
        public String render() {
            return "()";
        }
    }

    private static final class RecordValue implements Value {
        private final RecordType type;
        private final List<Value> fields;

        private RecordValue(RecordType type, List<Value> fields) {
            this.type = type;
            this.fields = List.copyOf(fields);
        }

        private RecordType type() {
            return type;
        }

        private Value field(int index) {
            return fields.get(index);
        }

        @Override
        public String render() {
            return "#<record " + type.name() + ">";
        }
    }

    private record VoidValue() implements Value {
        @Override
        public String render() {
            return "";
        }
    }

    private record UninitializedValue() implements Value {
        @Override
        public String render() {
            return "#<uninitialized>";
        }
    }

    private static final class BuiltinProcedure implements Value {
        private final String name;
        private final BuiltinImplementation implementation;

        private BuiltinProcedure(String name, BuiltinImplementation implementation) {
            this.name = name;
            this.implementation = implementation;
        }

        private Value apply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError {
            return implementation.apply(callPos, arguments);
        }

        @Override
        public String render() {
            return "#<procedure " + name + ">";
        }
    }

    private record LambdaProcedure(String name, ParameterSpec parameters, List<Expr> body,
                                   Environment closure) implements Value {
        private String displayName() {
            return name == null ? "lambda" : name;
        }

        @Override
        public String render() {
            return "#<procedure " + displayName() + ">";
        }
    }

    private record CaseLambdaProcedure(String name, List<CaseLambdaClause> clauses,
                                       Environment closure) implements Value {
        private String displayName() {
            return name == null ? "case-lambda" : name;
        }

        @Override
        public String render() {
            return "#<procedure " + displayName() + ">";
        }
    }

    private record CaseLambdaClause(ParameterSpec parameters, List<Expr> body) {}

    private record ParameterSpec(List<String> required, String rest) {
        private static ParameterSpec fixed(List<String> required) {
            return new ParameterSpec(List.copyOf(required), null);
        }

        private static ParameterSpec restOnly(String rest) {
            return new ParameterSpec(List.of(), rest);
        }

        private boolean accepts(int argumentCount) {
            if (rest != null) {
                return argumentCount >= required.size();
            }
            return argumentCount == required.size();
        }
    }

    private record Comparison(String name, Comparator comparator) {
        private static final Comparison LESS_THAN = new Comparison("<", ordering -> ordering < 0);
        private static final Comparison GREATER_THAN = new Comparison(">", ordering -> ordering > 0);
        private static final Comparison EQUAL = new Comparison("=", ordering -> ordering == 0);
        private static final Comparison LESS_EQUAL = new Comparison("<=", ordering -> ordering <= 0);
        private static final Comparison GREATER_EQUAL = new Comparison(">=", ordering -> ordering >= 0);

        private boolean test(int ordering) {
            return comparator.test(ordering);
        }
    }

    @FunctionalInterface
    private interface Comparator {
        boolean test(int ordering);
    }

    @FunctionalInterface
    private interface BuiltinImplementation {
        Value apply(SourcePos callPos, List<LocatedValue> arguments) throws EvalError;
    }

    private record Binding(String name, Expr valueExpr) {}

    private record DoBinding(String name, Expr initExpr, Expr stepExpr) {}

    private record RecordConstructorSpec(String name, List<String> fields) {}

    private record RecordAccessorSpec(String fieldName, String accessorName) {}

    private record ProgramEvaluation(Value result, String output) {}

    private record MacroDefinition(String name, Set<String> literals, List<MacroRule> rules,
                                   Environment definitionEnv, Set<String> definitionMacros) {}

    private record MacroRule(Expr pattern, Expr template) {}

    private static final class MacroBindings {
        private final Map<String, Expr> single = new HashMap<>();
        private final Map<String, List<Expr>> repeated = new HashMap<>();

        private boolean bindSingle(String name, Expr value) {
            Expr existing = single.get(name);
            if (existing != null) {
                return exprSyntaxEq(existing, value);
            }
            single.put(name, value);
            return true;
        }

        private void pushRepeated(String name, Expr value) {
            repeated.computeIfAbsent(name, ignored -> new ArrayList<>()).add(value);
        }

        private void ensureRepeated(String name) {
            repeated.computeIfAbsent(name, ignored -> new ArrayList<>());
        }

        private Expr lookup(String name, Integer repeatIndex) {
            List<Expr> repeatedValues = repeated.get(name);
            if (repeatedValues != null) {
                if (repeatIndex == null || repeatIndex < 0 || repeatIndex >= repeatedValues.size()) {
                    return null;
                }
                return repeatedValues.get(repeatIndex);
            }
            return single.get(name);
        }

        private Integer repeatedLen(String name) {
            List<Expr> repeatedValues = repeated.get(name);
            return repeatedValues == null ? null : repeatedValues.size();
        }
    }

    private record CaptureBinding(String name, Value value) {}

    private record MacroExpansion(Expr expr, List<CaptureBinding> captureBindings) {}

    private static final class MacroExpansionState {
        private final Map<String, String> captureAliases = new HashMap<>();
        private final List<CaptureBinding> captureBindings = new ArrayList<>();
    }

    private static final class RecordType {
        private final String name;
        private final List<String> fields;
        private final Map<String, Integer> fieldIndexes = new HashMap<>();

        private RecordType(String name, List<String> fields) {
            this.name = name;
            this.fields = List.copyOf(fields);
            for (int i = 0; i < fields.size(); i++) {
                if (fieldIndexes.putIfAbsent(fields.get(i), i) != null) {
                    throw new IllegalArgumentException("duplicate record field");
                }
            }
        }

        private String name() {
            return name;
        }

        private int fieldCount() {
            return fields.size();
        }

        private Integer fieldIndex(String fieldName) {
            return fieldIndexes.get(fieldName);
        }
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
                Value value = bindings.get(name);
                if (value instanceof UninitializedValue) {
                    throw new EvalError("uninitialized binding: " + name);
                }
                return value;
            }
            if (parent != null) {
                return parent.lookup(name);
            }
            throw new EvalError("unbound symbol: " + name);
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
            throw new EvalError("unbound symbol: " + name);
        }
    }

    private static boolean isSpecialForm(String name) {
        return switch (name) {
            case "and", "begin", "case", "case-lambda", "cond", "define", "define-record-type", "define-syntax",
                    "do", "else", "if", "lambda", "let", "letrec", "letrec*", "or", "quote", "set!",
                    "syntax-rules" -> true;
            default -> false;
        };
    }

    private static boolean isProcedureValue(Value value) {
        return value instanceof BuiltinProcedure
                || value instanceof LambdaProcedure
                || value instanceof CaseLambdaProcedure;
    }

    private static boolean isEllipsisExpr(Expr expr) {
        return expr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("...");
    }

    private static boolean isPatternVariable(String name, String macroName, Set<String> literals) {
        return !name.equals("...") && !name.equals(macroName) && !literals.contains(name);
    }

    private static MacroBindings matchMacroPattern(Expr pattern, Expr input, String macroName,
                                                   Set<String> literals) {
        MacroBindings bindings = new MacroBindings();
        return matchPattern(pattern, input, macroName, literals, bindings, false) ? bindings : null;
    }

    private static boolean matchPattern(Expr pattern, Expr input, String macroName,
                                        Set<String> literals, MacroBindings bindings,
                                        boolean repeated) {
        if (pattern instanceof SymbolExpr symbolPattern) {
            if (isPatternVariable(symbolPattern.name(), macroName, literals)) {
                if (repeated) {
                    bindings.pushRepeated(symbolPattern.name(), input);
                    return true;
                }
                return bindings.bindSingle(symbolPattern.name(), input);
            }
            return input instanceof SymbolExpr symbolInput
                    && symbolInput.name().equals(symbolPattern.name());
        }

        if (pattern instanceof NumberExpr || pattern instanceof BoolExpr
                || pattern instanceof StringExpr || pattern instanceof CharExpr) {
            return exprSyntaxEq(pattern, input);
        }

        if (pattern instanceof VectorExpr vectorPattern && input instanceof VectorExpr vectorInput) {
            return matchListPattern(
                    vectorPattern.elements(),
                    vectorInput.elements(),
                    macroName,
                    literals,
                    bindings);
        }

        if (pattern instanceof ListExpr listPattern && input instanceof ListExpr listInput) {
            return matchListPattern(
                    listPattern.elements(),
                    listInput.elements(),
                    macroName,
                    literals,
                    bindings);
        }
        return false;
    }

    private static boolean matchListPattern(List<Expr> patterns, List<Expr> inputs,
                                            String macroName, Set<String> literals,
                                            MacroBindings bindings) {
        int patternIndex = 0;
        int inputIndex = 0;

        while (patternIndex < patterns.size()) {
            if (patternIndex + 1 < patterns.size() && isEllipsisExpr(patterns.get(patternIndex + 1))) {
                int suffixMin = minPatternInputCount(patterns.subList(patternIndex + 2, patterns.size()));
                if (inputs.size() < inputIndex + suffixMin) {
                    return false;
                }

                int repeatCount = inputs.size() - inputIndex - suffixMin;
                initializeRepeatedBindings(patterns.get(patternIndex), macroName, literals, bindings);
                for (int i = 0; i < repeatCount; i++) {
                    if (!matchPattern(
                            patterns.get(patternIndex),
                            inputs.get(inputIndex + i),
                            macroName,
                            literals,
                            bindings,
                            true)) {
                        return false;
                    }
                }

                inputIndex += repeatCount;
                patternIndex += 2;
                continue;
            }

            if (inputIndex >= inputs.size() || !matchPattern(
                    patterns.get(patternIndex),
                    inputs.get(inputIndex),
                    macroName,
                    literals,
                    bindings,
                    false)) {
                return false;
            }

            patternIndex++;
            inputIndex++;
        }

        return inputIndex == inputs.size();
    }

    private static int minPatternInputCount(List<Expr> patterns) {
        int count = 0;
        for (int index = 0; index < patterns.size(); ) {
            if (index + 1 < patterns.size() && isEllipsisExpr(patterns.get(index + 1))) {
                index += 2;
            } else {
                count++;
                index++;
            }
        }
        return count;
    }

    private static void initializeRepeatedBindings(Expr pattern, String macroName,
                                                   Set<String> literals, MacroBindings bindings) {
        if (pattern instanceof SymbolExpr symbolPattern) {
            if (isPatternVariable(symbolPattern.name(), macroName, literals)) {
                bindings.ensureRepeated(symbolPattern.name());
            }
            return;
        }
        if (pattern instanceof ListExpr listPattern) {
            for (Expr element : listPattern.elements()) {
                initializeRepeatedBindings(element, macroName, literals, bindings);
            }
            return;
        }
        if (pattern instanceof VectorExpr vectorPattern) {
            for (Expr element : vectorPattern.elements()) {
                initializeRepeatedBindings(element, macroName, literals, bindings);
            }
        }
    }

    private static int repetitionCountForTemplate(Expr template, MacroBindings bindings)
            throws EvalError {
        CountHolder count = new CountHolder();
        collectRepetitionCount(template, bindings, count);
        if (count.value == null) {
            throw new EvalError("invalid syntax-rules template");
        }
        return count.value;
    }

    private static void collectRepetitionCount(Expr template, MacroBindings bindings,
                                               CountHolder count) throws EvalError {
        if (template instanceof SymbolExpr symbolExpr) {
            Integer len = bindings.repeatedLen(symbolExpr.name());
            if (len != null) {
                if (count.value != null && !count.value.equals(len)) {
                    throw new EvalError("invalid syntax-rules template");
                }
                count.value = len;
            }
            return;
        }
        if (template instanceof ListExpr listExpr) {
            for (Expr element : listExpr.elements()) {
                collectRepetitionCount(element, bindings, count);
            }
            return;
        }
        if (template instanceof VectorExpr vectorExpr) {
            for (Expr element : vectorExpr.elements()) {
                collectRepetitionCount(element, bindings, count);
            }
        }
    }

    private static boolean exprSyntaxEq(Expr left, Expr right) {
        if (left instanceof NumberExpr leftNumber && right instanceof NumberExpr rightNumber) {
            return sameNumberLiteral(leftNumber.value(), rightNumber.value());
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
        if (left instanceof VectorExpr leftVector && right instanceof VectorExpr rightVector) {
            if (leftVector.elements().size() != rightVector.elements().size()) {
                return false;
            }
            for (int i = 0; i < leftVector.elements().size(); i++) {
                if (!exprSyntaxEq(leftVector.elements().get(i), rightVector.elements().get(i))) {
                    return false;
                }
            }
            return true;
        }
        if (left instanceof SymbolExpr leftSymbol && right instanceof SymbolExpr rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof ListExpr leftList && right instanceof ListExpr rightList) {
            if (leftList.elements().size() != rightList.elements().size()) {
                return false;
            }
            for (int i = 0; i < leftList.elements().size(); i++) {
                if (!exprSyntaxEq(leftList.elements().get(i), rightList.elements().get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private static final class CountHolder {
        private Integer value;
    }

    private static final BoolValue TRUE_VALUE = new BoolValue(true);
    private static final BoolValue FALSE_VALUE = new BoolValue(false);
    private static final EmptyListValue EMPTY_LIST = new EmptyListValue();
    private static final UninitializedValue UNINITIALIZED_VALUE = new UninitializedValue();
    private static final VoidValue VOID_VALUE = new VoidValue();

    private static String renderValue(Value value, boolean displayMode) {
        if (value instanceof PairValue pair) {
            return renderPair(pair, displayMode);
        }
        if (value instanceof VectorValue vector) {
            return renderVector(vector, displayMode);
        }
        if (displayMode) {
            if (value instanceof StringValue stringValue) {
                return stringValue.text();
            }
            if (value instanceof CharValue(char ch)) {
                return Character.toString(ch);
            }
        }
        return value.render();
    }

    private static String renderPair(PairValue pair, boolean displayMode) {
        StringBuilder builder = new StringBuilder("(");
        Value current = pair;
        boolean first = true;

        while (current instanceof PairValue(Value car, Value cdr)) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(renderValue(car, displayMode));
            current = cdr;
            first = false;
        }

        if (current instanceof EmptyListValue) {
            builder.append(')');
            return builder.toString();
        }

        builder.append(" . ").append(renderValue(current, displayMode)).append(')');
        return builder.toString();
    }

    private static String renderVector(VectorValue vector, boolean displayMode) {
        StringBuilder builder = new StringBuilder("#(");
        for (int i = 0; i < vector.length(); i++) {
            if (i > 0) {
                builder.append(' ');
            }
            builder.append(renderValue(vector.element(i), displayMode));
        }
        builder.append(')');
        return builder.toString();
    }

    private static String renderStringLiteral(String value) {
        String escaped = value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
        return "\"" + escaped + "\"";
    }

    private static String renderCharacterLiteral(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    private static final class Parser {
        private final String input;
        private final List<Integer> lineStarts;
        private int index;

        private Parser(String input) {
            this.input = input;
            this.lineStarts = computeLineStarts(input);
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipWhitespace();
            while (!isAtEnd()) {
                expressions.add(parseExpr());
                skipWhitespace();
            }
            return expressions;
        }

        private Expr parseExpr() throws EvalError {
            skipWhitespace();
            if (isAtEnd()) {
                throw errorAtCurrent("unexpected end of input");
            }

            SourcePos pos = positionAt(index);
            char ch = input.charAt(index);
            if (ch == '\'') {
                return parseQuoted(pos);
            }
            if (ch == '(') {
                return parseList(pos);
            }
            if (ch == ')') {
                throw errorAtCurrent("unexpected )");
            }
            if (ch == '"') {
                return parseString(pos);
            }
            if (ch == '#') {
                return parseBooleanOrCharacter(pos);
            }
            return parseAtom(pos);
        }

        private Expr parseQuoted(SourcePos pos) throws EvalError {
            index++;
            return new ListExpr(List.of(new SymbolExpr("quote", pos), parseExpr()), pos);
        }

        private Expr parseList(SourcePos pos) throws EvalError {
            index++;
            List<Expr> elements = new ArrayList<>();
            skipWhitespace();

            while (true) {
                if (isAtEnd()) {
                    throw errorAt(pos, "unterminated list");
                }
                if (input.charAt(index) == ')') {
                    index++;
                    return new ListExpr(elements, pos);
                }
                elements.add(parseExpr());
                skipWhitespace();
            }
        }

        private Expr parseString(SourcePos pos) throws EvalError {
            index++;
            StringBuilder value = new StringBuilder();
            while (!isAtEnd()) {
                char ch = input.charAt(index++);
                if (ch == '"') {
                    return new StringExpr(value.toString(), pos);
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw errorAt(pos, "unterminated string");
                    }
                    char escaped = input.charAt(index++);
                    value.append(switch (escaped) {
                        case 'n' -> '\n';
                        case 't' -> '\t';
                        case '"' -> '"';
                        case '\\' -> '\\';
                        default -> escaped;
                    });
                } else {
                    value.append(ch);
                }
            }
            throw errorAt(pos, "unterminated string");
        }

        private Expr parseBooleanOrCharacter(SourcePos pos) throws EvalError {
            if (matchesToken("#t")) {
                index += 2;
                return new BoolExpr(true, pos);
            }
            if (matchesToken("#f")) {
                index += 2;
                return new BoolExpr(false, pos);
            }
            if (input.startsWith("#(", index)) {
                return parseVectorLiteral(pos);
            }
            if (input.startsWith("#\\", index)) {
                return parseCharacter(pos);
            }
            throw errorAt(pos, "invalid boolean literal");
        }

        private Expr parseVectorLiteral(SourcePos pos) throws EvalError {
            index += 2;
            List<Expr> elements = new ArrayList<>();
            skipWhitespace();

            while (true) {
                if (isAtEnd()) {
                    throw errorAt(pos, "unterminated vector");
                }
                if (input.charAt(index) == ')') {
                    index++;
                    return new VectorExpr(elements, pos);
                }
                elements.add(parseExpr());
                skipWhitespace();
            }
        }

        private Expr parseCharacter(SourcePos pos) throws EvalError {
            index += 2;
            int start = index;
            while (!isAtEnd() && !isDelimiter(input.charAt(index))) {
                index++;
            }

            String token = input.substring(start, index);
            if (token.isEmpty()) {
                throw errorAt(pos, "invalid character literal");
            }
            if (token.length() == 1) {
                return new CharExpr(token.charAt(0), pos);
            }
            return switch (token) {
                case "space" -> new CharExpr(' ', pos);
                case "newline" -> new CharExpr('\n', pos);
                default -> throw errorAt(pos, "invalid character literal");
            };
        }

        private boolean matchesToken(String token) {
            int end = index + token.length();
            if (end > input.length()) {
                return false;
            }
            if (!input.startsWith(token, index)) {
                return false;
            }
            return end == input.length() || isDelimiter(input.charAt(end));
        }

        private Expr parseAtom(SourcePos pos) throws EvalError {
            int start = index;
            while (!isAtEnd() && !isDelimiter(input.charAt(index))) {
                index++;
            }

            String token = input.substring(start, index);
            if (token.isEmpty()) {
                throw errorAt(pos, "unexpected token");
            }

            try {
                NumberValue number = parseNumberLiteral(token);
                if (number != null) {
                    return new NumberExpr(number, pos);
                }
            } catch (IllegalArgumentException e) {
                throw errorAt(pos, e.getMessage());
            }
            return new SymbolExpr(token, pos);
        }

        private void skipWhitespace() {
            while (!isAtEnd()) {
                char ch = input.charAt(index);
                if (Character.isWhitespace(ch)) {
                    index++;
                    continue;
                }
                if (ch == ';') {
                    while (!isAtEnd() && input.charAt(index) != '\n') {
                        index++;
                    }
                    continue;
                }
                return;
            }
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private EvalError errorAtCurrent(String message) {
            return errorAt(positionAt(index), message);
        }

        private EvalError errorAt(SourcePos pos, String message) {
            return new EvalError(message, pos.line(), pos.column());
        }

        private SourcePos positionAt(int absoluteIndex) {
            int cappedIndex = Math.max(0, Math.min(absoluteIndex, input.length()));
            int low = 0;
            int high = lineStarts.size() - 1;
            int lineIndex = 0;

            while (low <= high) {
                int mid = (low + high) >>> 1;
                int lineStart = lineStarts.get(mid);
                if (lineStart <= cappedIndex) {
                    lineIndex = mid;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }

            int lineStart = lineStarts.get(lineIndex);
            return new SourcePos(lineIndex + 1, cappedIndex - lineStart + 1);
        }

        private List<Integer> computeLineStarts(String source) {
            List<Integer> starts = new ArrayList<>();
            starts.add(0);
            for (int i = 0; i < source.length(); i++) {
                if (source.charAt(i) == '\n') {
                    starts.add(i + 1);
                }
            }
            return starts;
        }
    }
}
