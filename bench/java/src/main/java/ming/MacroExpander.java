package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

final class MacroExpander {
    @FunctionalInterface
    interface ProcedureInvoker {
        Value apply(Value operator, List<Value> arguments) throws EvalError;
    }

    private final ProcedureInvoker procedureInvoker;
    private final Map<String, MacroValue> macros = new HashMap<>();
    private final Map<String, Cell> capturedBindings = new HashMap<>();
    private long generatedNameCounter;

    MacroExpander(ProcedureInvoker procedureInvoker) {
        this.procedureInvoker = procedureInvoker;
    }

    void defineSyntax(List<Expr> arguments, Env env) throws EvalError {
        requireExactArgs("define-syntax", arguments, 2);

        Expr target = arguments.get(0);
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("define-syntax target must be a symbol");
        }

        macros.put(symbolExpr.name(), parseMacro(symbolExpr.name(), arguments.get(1), env));
    }

    Optional<Expr> expandInvocation(String operatorName, ListExpr invocation) throws EvalError {
        MacroValue macro = macros.get(operatorName);
        if (macro == null) {
            return Optional.empty();
        }
        return Optional.of(expandMacro(invocation, macro));
    }

    Value lookupSymbol(String name, Env env) throws EvalError {
        Cell capturedCell = capturedBindings.get(name);
        if (capturedCell != null) {
            if (capturedCell.isUninitialized()) {
                throw new EvalError("unbound variable: " + name);
            }
            return capturedCell.get();
        }
        return env.lookup(name);
    }

    void setSymbol(String name, Value value, Env env) throws EvalError {
        Cell capturedCell = capturedBindings.get(name);
        if (capturedCell != null) {
            if (capturedCell.isUninitialized()) {
                throw new EvalError("unbound variable: " + name);
            }
            capturedCell.set(value);
            return;
        }
        env.set(name, value);
    }

    private MacroValue parseMacro(String name, Expr transformerExpr, Env env)
            throws EvalError {
        if (!(transformerExpr instanceof ListExpr transformerList)) {
            throw new EvalError("define-syntax expects a syntax-rules or syntax-case transformer");
        }

        List<Expr> transformer = transformerList.elements();
        String transformerName = transformer.isEmpty() ? null : symbolName(transformer.get(0));
        if ("syntax-rules".equals(transformerName)) {
            return parseSyntaxRulesMacro(name, transformer, env);
        }
        if ("lambda".equals(transformerName)) {
            return parseSyntaxCaseMacro(name, transformer, env);
        }
        throw new EvalError("define-syntax expects a syntax-rules or syntax-case transformer");
    }

    private MacroValue parseSyntaxRulesMacro(String name, List<Expr> transformer, Env env)
            throws EvalError {
        if (transformer.size() < 3) {
            throw new EvalError("define-syntax expects a syntax-rules transformer");
        }

        Set<String> literals = parseLiteralIdentifiers(transformer.get(1));
        List<MacroRule> rules = new ArrayList<>();
        for (int index = 2; index < transformer.size(); index++) {
            Expr ruleExpr = transformer.get(index);
            if (!(ruleExpr instanceof ListExpr ruleList) || ruleList.elements().size() != 2) {
                throw new EvalError("syntax-rules clause must contain a pattern and template");
            }
            rules.add(new MacroRule(ruleList.elements().get(0), null, ruleList.elements().get(1)));
        }

        return new MacroValue(name, Set.copyOf(literals), List.copyOf(rules), env, false);
    }

    private MacroValue parseSyntaxCaseMacro(String name, List<Expr> transformer, Env env)
            throws EvalError {
        if (transformer.size() != 3) {
            throw new EvalError("syntax-case transformer must have exactly one body expression");
        }

        String parameterName = parseSyntaxCaseParameter(transformer.get(1));
        if (!(transformer.get(2) instanceof ListExpr syntaxCaseExpr)) {
            throw new EvalError("syntax-case transformer must contain a syntax-case expression");
        }

        List<Expr> syntaxCaseElements = syntaxCaseExpr.elements();
        if (syntaxCaseElements.size() < 4
                || !"syntax-case".equals(symbolName(syntaxCaseElements.get(0)))) {
            throw new EvalError("syntax-case transformer must contain a syntax-case expression");
        }
        if (!(syntaxCaseElements.get(1) instanceof SymbolExpr inputSymbol)
                || !parameterName.equals(inputSymbol.name())) {
            throw new EvalError("syntax-case must analyze the transformer's input parameter");
        }

        Set<String> literals = parseLiteralIdentifiers(syntaxCaseElements.get(2));
        List<MacroRule> rules = new ArrayList<>();
        for (int index = 3; index < syntaxCaseElements.size(); index++) {
            Expr ruleExpr = syntaxCaseElements.get(index);
            if (!(ruleExpr instanceof ListExpr ruleList)) {
                throw new EvalError("syntax-case clause must be a list");
            }

            List<Expr> clause = ruleList.elements();
            if (clause.size() < 2 || clause.size() > 3) {
                throw new EvalError("syntax-case clause must contain a pattern and result");
            }

            Expr guard = clause.size() == 3 ? clause.get(1) : null;
            Expr result = clause.get(clause.size() - 1);
            rules.add(new MacroRule(clause.get(0), guard, result));
        }

        return new MacroValue(name, Set.copyOf(literals), List.copyOf(rules), env, true);
    }

    private String parseSyntaxCaseParameter(Expr parameterExpr) throws EvalError {
        if (!(parameterExpr instanceof ListExpr parameterList)
                || parameterList.elements().size() != 1
                || !(parameterList.elements().get(0) instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("syntax-case transformer must accept exactly one parameter");
        }
        return symbolExpr.name();
    }

    private Set<String> parseLiteralIdentifiers(Expr literalExpr) throws EvalError {
        if (!(literalExpr instanceof ListExpr literalList)) {
            throw new EvalError("syntax-rules literals must be a list");
        }

        Set<String> literals = new HashSet<>();
        for (Expr entry : literalList.elements()) {
            if (!(entry instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("syntax-rules literal identifiers must be symbols");
            }
            literals.add(symbolExpr.name());
        }
        return literals;
    }

    private Expr expandMacro(ListExpr invocation, MacroValue macro) throws EvalError {
        for (MacroRule rule : macro.rules()) {
            MatchBindings bindings = new MatchBindings();
            if (matchPattern(rule.pattern(), invocation, macro.literals(),
                    macro.name(), bindings, false)) {
                if (macro.syntaxCase()) {
                    if (rule.guard() != null) {
                        Value guardValue = requireTransformerDatum(
                                evaluateTransformerExpression(rule.guard(), macro, bindings),
                                "syntax-case guard");
                        if (!EvaluatorSupport.isTruthy(guardValue)) {
                            continue;
                        }
                    }
                    return requireTransformerSyntax(
                            evaluateTransformerExpression(rule.result(), macro, bindings),
                            "syntax-case result");
                }
                return expandTemplate(rule.result(), macro, bindings, new HashMap<>(), null);
            }
        }
        throw new EvalError("macro invocation did not match any macro clause");
    }

    private boolean matchPattern(Expr pattern, Expr input, Set<String> literals,
            String macroName, MatchBindings bindings, boolean repeatedContext)
            throws EvalError {
        String patternName = symbolName(pattern);
        if (patternName != null) {
            if ("...".equals(patternName)) {
                throw new EvalError("ellipsis cannot appear by itself in a pattern");
            }
            if ("_".equals(patternName)) {
                return true;
            }
            if (patternName.equals(macroName) || literals.contains(patternName)) {
                String inputName = symbolName(input);
                return inputName != null && inputName.equals(patternName);
            }
            if (repeatedContext) {
                bindings.addRepeated(patternName, input);
                return true;
            }
            return bindings.addSingle(patternName, input);
        }

        return switch (pattern) {
            case SymbolExpr ignored -> throw new IllegalStateException(
                    "symbol patterns are handled before the switch");
            case IntExpr intExpr -> input instanceof IntExpr other
                    && intExpr.value() == other.value();
            case RationalExpr rationalExpr -> input instanceof RationalExpr other
                    && rationalExpr.numerator() == other.numerator()
                    && rationalExpr.denominator() == other.denominator();
            case InexactExpr inexactExpr -> input instanceof InexactExpr other
                    && Double.compare(inexactExpr.value(), other.value()) == 0;
            case BoolExpr boolExpr -> input instanceof BoolExpr other
                    && boolExpr.value() == other.value();
            case CharExpr charExpr -> input instanceof CharExpr other
                    && charExpr.value() == other.value();
            case StringExpr stringExpr -> input instanceof StringExpr other
                    && stringExpr.value().equals(other.value());
            case ListExpr listPattern -> input instanceof ListExpr listInput
                    && matchPatternList(listPattern.elements(), listInput.elements(),
                    literals, macroName, bindings, repeatedContext);
        };
    }

    private boolean matchPatternList(List<Expr> patternElements, List<Expr> inputElements,
            Set<String> literals, String macroName, MatchBindings bindings,
            boolean repeatedContext) throws EvalError {
        int patternIndex = 0;
        int inputIndex = 0;

        while (patternIndex < patternElements.size()) {
            if (patternIndex + 1 < patternElements.size()
                    && isEllipsis(patternElements.get(patternIndex + 1))) {
                Expr repeatedPattern = patternElements.get(patternIndex);
                bindings.ensureRepeated(collectPatternVariables(repeatedPattern, literals,
                        macroName));

                int minimumRemaining = minimumPatternLength(patternElements, patternIndex + 2);
                int repeatCount = inputElements.size() - inputIndex - minimumRemaining;
                if (repeatCount < 0) {
                    return false;
                }

                for (int count = 0; count < repeatCount; count++) {
                    if (!matchPattern(repeatedPattern, inputElements.get(inputIndex),
                            literals, macroName, bindings, true)) {
                        return false;
                    }
                    inputIndex++;
                }
                patternIndex += 2;
                continue;
            }

            if (inputIndex >= inputElements.size()) {
                return false;
            }
            if (!matchPattern(patternElements.get(patternIndex), inputElements.get(inputIndex),
                    literals, macroName, bindings, repeatedContext)) {
                return false;
            }
            patternIndex++;
            inputIndex++;
        }

        return inputIndex == inputElements.size();
    }

    private int minimumPatternLength(List<Expr> patternElements, int startIndex) {
        int remaining = 0;
        for (int index = startIndex; index < patternElements.size(); index++) {
            if (index + 1 < patternElements.size() && isEllipsis(patternElements.get(index + 1))) {
                index++;
                continue;
            }
            remaining++;
        }
        return remaining;
    }

    private Set<String> collectPatternVariables(Expr pattern, Set<String> literals,
            String macroName) {
        Set<String> variables = new HashSet<>();
        collectPatternVariables(pattern, literals, macroName, variables);
        return variables;
    }

    private void collectPatternVariables(Expr pattern, Set<String> literals,
            String macroName, Set<String> variables) {
        String patternName = symbolName(pattern);
        if (patternName != null) {
            if (!"...".equals(patternName)
                    && !"_".equals(patternName)
                    && !patternName.equals(macroName)
                    && !literals.contains(patternName)) {
                variables.add(patternName);
            }
            return;
        }

        if (pattern instanceof ListExpr listPattern) {
            for (Expr element : listPattern.elements()) {
                if (!isEllipsis(element)) {
                    collectPatternVariables(element, literals, macroName, variables);
                }
            }
        }
    }

    private TransformerResult evaluateTransformerExpression(Expr expr, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        String name = symbolName(expr);
        if (name != null) {
            return lookupTransformerSymbol(name, macro, bindings);
        }

        return switch (expr) {
            case SymbolExpr ignored -> throw new IllegalStateException(
                    "symbol transformer expressions are handled before the switch");
            case IntExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case RationalExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case InexactExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case BoolExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case CharExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case StringExpr ignored -> new TransformerDatum(syntaxToDatum(expr));
            case ListExpr listExpr -> evaluateTransformerList(listExpr, macro, bindings);
        };
    }

    private TransformerResult lookupTransformerSymbol(String name, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        Expr singleBinding = bindings.single(name);
        if (singleBinding != null) {
            return new TransformerSyntax(singleBinding);
        }

        List<Expr> repeatedBinding = bindings.repeated(name);
        if (repeatedBinding != null) {
            if (repeatedBinding.size() != 1) {
                throw new EvalError(
                        "transformer expression references repeated pattern variable without ellipsis");
            }
            return new TransformerSyntax(repeatedBinding.get(0));
        }

        return new TransformerDatum(macro.definitionEnv().lookup(name));
    }

    private TransformerResult evaluateTransformerList(ListExpr listExpr, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty transformer expression");
        }

        String operatorName = symbolName(elements.get(0));
        if ("syntax".equals(operatorName)) {
            return evaluateSyntaxForm(elements, macro, bindings);
        }
        if ("with-syntax".equals(operatorName)) {
            return evaluateWithSyntax(elements, macro, bindings);
        }
        if ("syntax->datum".equals(operatorName)) {
            return evaluateSyntaxToDatum(elements, macro, bindings);
        }
        if ("datum->syntax".equals(operatorName)) {
            return evaluateDatumToSyntax(elements, macro, bindings);
        }
        if ("quote".equals(operatorName)) {
            return evaluateTransformerQuote(elements);
        }

        TransformerResult operatorResult = evaluateTransformerExpression(elements.get(0), macro,
                bindings);
        Value operator = requireTransformerDatum(operatorResult, "transformer application");
        List<Value> arguments = new ArrayList<>(elements.size() - 1);
        for (int index = 1; index < elements.size(); index++) {
            arguments.add(requireTransformerDatum(
                    evaluateTransformerExpression(elements.get(index), macro, bindings),
                    "transformer application"));
        }
        return new TransformerDatum(procedureInvoker.apply(operator, arguments));
    }

    private TransformerResult evaluateSyntaxForm(List<Expr> elements, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        requireExactArgs("syntax", elements.subList(1, elements.size()), 1);
        return new TransformerSyntax(
                expandTemplate(elements.get(1), macro, bindings, new HashMap<>(), null));
    }

    private TransformerResult evaluateWithSyntax(List<Expr> elements, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("with-syntax expects a binding list and a body");
        }
        if (!(elements.get(1) instanceof ListExpr bindingList)) {
            throw new EvalError("with-syntax expects a binding list");
        }

        MatchBindings localBindings = bindings.copy();
        for (Expr bindingExpr : bindingList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingEntry)
                    || bindingEntry.elements().size() != 2) {
                throw new EvalError("with-syntax bindings must contain a name and expression");
            }
            if (!(bindingEntry.elements().get(0) instanceof SymbolExpr bindingName)) {
                throw new EvalError("with-syntax binding names must be symbols");
            }

            Expr syntaxValue = requireTransformerSyntax(
                    evaluateTransformerExpression(bindingEntry.elements().get(1), macro, bindings),
                    "with-syntax binding");
            localBindings.putSingle(bindingName.name(), syntaxValue);
        }

        TransformerResult result = null;
        for (int index = 2; index < elements.size(); index++) {
            result = evaluateTransformerExpression(elements.get(index), macro, localBindings);
        }
        if (result == null) {
            throw new EvalError("with-syntax requires a body");
        }
        return result;
    }

    private TransformerResult evaluateSyntaxToDatum(List<Expr> elements, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        requireExactArgs("syntax->datum", elements.subList(1, elements.size()), 1);
        Expr syntaxExpr = requireTransformerSyntax(
                evaluateTransformerExpression(elements.get(1), macro, bindings),
                "syntax->datum");
        return new TransformerDatum(syntaxToDatum(syntaxExpr));
    }

    private TransformerResult evaluateDatumToSyntax(List<Expr> elements, MacroValue macro,
            MatchBindings bindings) throws EvalError {
        requireExactArgs("datum->syntax", elements.subList(1, elements.size()), 2);
        Expr contextSyntax = requireTransformerSyntax(
                evaluateTransformerExpression(elements.get(1), macro, bindings),
                "datum->syntax");
        Value datum = requireTransformerDatum(
                evaluateTransformerExpression(elements.get(2), macro, bindings),
                "datum->syntax");
        return new TransformerSyntax(
                datumToSyntax(datum, contextSyntax.line(), contextSyntax.column()));
    }

    private TransformerResult evaluateTransformerQuote(List<Expr> elements) throws EvalError {
        requireExactArgs("quote", elements.subList(1, elements.size()), 1);
        return new TransformerDatum(syntaxToDatum(elements.get(1)));
    }

    private Expr requireTransformerSyntax(TransformerResult result, String operation)
            throws EvalError {
        if (result instanceof TransformerSyntax syntaxResult) {
            return syntaxResult.expr();
        }
        throw new EvalError(operation + " expects a syntax object");
    }

    private Value requireTransformerDatum(TransformerResult result, String operation)
            throws EvalError {
        if (result instanceof TransformerDatum datumResult) {
            return datumResult.value();
        }
        throw new EvalError(operation + " expects a datum");
    }

    private Value syntaxToDatum(Expr expr) {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> ValueSupport.exactValue(
                    rationalExpr.numerator(), rationalExpr.denominator());
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> RuntimeConstants.boolValue(boolExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case StringExpr stringExpr -> ValueSupport.immutableString(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> EvaluatorSupport.listValue(
                    syntaxElementsToDatum(listExpr.elements()));
        };
    }

    private List<Value> syntaxElementsToDatum(List<Expr> expressions) {
        List<Value> values = new ArrayList<>(expressions.size());
        for (Expr expression : expressions) {
            values.add(syntaxToDatum(expression));
        }
        return values;
    }

    private Expr datumToSyntax(Value value, int line, int column) throws EvalError {
        return switch (value) {
            case IntValue intValue -> new IntExpr(intValue.value(), line, column);
            case RationalValue rationalValue -> new RationalExpr(
                    rationalValue.numerator(), rationalValue.denominator(), line, column);
            case InexactValue inexactValue -> new InexactExpr(inexactValue.value(), line, column);
            case BoolValue boolValue -> new BoolExpr(boolValue.value(), line, column);
            case CharValue charValue -> new CharExpr(charValue.value(), line, column);
            case StringValue stringValue -> new StringExpr(stringValue.text(), line, column);
            case SymbolValue symbolValue -> new SymbolExpr(symbolValue.name(), line, column);
            case EmptyListValue ignored -> new ListExpr(List.of(), line, column);
            case PairValue ignored -> new ListExpr(datumElementsToSyntax(value, line, column),
                    line, column);
            default -> throw new EvalError("datum->syntax expects a symbol, list, string, "
                    + "character, boolean, or number");
        };
    }

    private List<Expr> datumElementsToSyntax(Value value, int line, int column)
            throws EvalError {
        List<Value> datumElements = EvaluatorSupport.requireProperList(value, "datum->syntax");
        List<Expr> syntaxElements = new ArrayList<>(datumElements.size());
        for (Value element : datumElements) {
            syntaxElements.add(datumToSyntax(element, line, column));
        }
        return syntaxElements;
    }

    private Expr expandTemplate(Expr template, MacroValue macro, MatchBindings bindings,
            Map<String, String> renamedBindings, Integer repetitionIndex) throws EvalError {
        return switch (template) {
            case SymbolExpr symbolExpr -> expandTemplateSymbol(symbolExpr, macro, bindings,
                    renamedBindings, repetitionIndex);
            case ListExpr listExpr -> expandTemplateList(listExpr, macro, bindings,
                    renamedBindings, repetitionIndex);
            case IntExpr ignored -> template;
            case RationalExpr ignored -> template;
            case InexactExpr ignored -> template;
            case BoolExpr ignored -> template;
            case CharExpr ignored -> template;
            case StringExpr ignored -> template;
        };
    }

    private Expr expandTemplateSymbol(SymbolExpr symbolExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Integer repetitionIndex) throws EvalError {
        String name = symbolExpr.name();

        Expr singleBinding = bindings.single(name);
        if (singleBinding != null) {
            return singleBinding;
        }

        List<Expr> repeatedBinding = bindings.repeated(name);
        if (repeatedBinding != null) {
            if (repetitionIndex == null) {
                throw new EvalError("template uses repeated pattern variable without ellipsis");
            }
            return repeatedBinding.get(repetitionIndex);
        }

        String renamed = renamedBindings.get(name);
        if (renamed != null) {
            return new SymbolExpr(renamed, symbolExpr.line(), symbolExpr.column());
        }

        if (isSyntaxKeyword(name)) {
            return symbolExpr;
        }

        Cell cell = macro.definitionEnv().lookupCell(name);
        if (cell != null && !cell.isUninitialized()) {
            return new SymbolExpr(captureDefinitionSiteName(name, cell),
                    symbolExpr.line(), symbolExpr.column());
        }
        return symbolExpr;
    }

    private Expr expandTemplateList(ListExpr listExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        String headName = elements.isEmpty() ? null : symbolName(elements.get(0));
        if ("let".equals(headName) && !bindings.isPatternVariable(headName)) {
            return expandLetTemplate(listExpr, macro, bindings, renamedBindings, repetitionIndex);
        }
        if ("lambda".equals(headName) && !bindings.isPatternVariable(headName)) {
            return expandLambdaTemplate(listExpr, macro, bindings, renamedBindings,
                    repetitionIndex);
        }
        return expandGenericTemplateList(listExpr, macro, bindings, renamedBindings,
                repetitionIndex);
    }

    private Expr expandGenericTemplateList(ListExpr listExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Integer repetitionIndex) throws EvalError {
        List<Expr> expandedElements = new ArrayList<>();
        List<Expr> elements = listExpr.elements();

        for (int index = 0; index < elements.size(); index++) {
            Expr element = elements.get(index);
            if (index + 1 < elements.size() && isEllipsis(elements.get(index + 1))) {
                int repeatCount = templateRepeatCount(element, bindings);
                for (int repetition = 0; repetition < repeatCount; repetition++) {
                    expandedElements.add(expandTemplate(element, macro, bindings,
                            renamedBindings, repetition));
                }
                index++;
                continue;
            }
            expandedElements.add(expandTemplate(element, macro, bindings,
                    renamedBindings, repetitionIndex));
        }

        return new ListExpr(List.copyOf(expandedElements), listExpr.line(), listExpr.column());
    }

    private Expr expandLetTemplate(ListExpr listExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.size() < 3 || !(elements.get(1) instanceof ListExpr bindingList)) {
            return expandGenericTemplateList(listExpr, macro, bindings, renamedBindings,
                    repetitionIndex);
        }

        List<Expr> expandedElements = new ArrayList<>(elements.size());
        expandedElements.add(expandTemplate(elements.get(0), macro, bindings, renamedBindings,
                repetitionIndex));

        Map<String, String> bodyRenames = new HashMap<>(renamedBindings);
        List<Expr> expandedBindings = new ArrayList<>(bindingList.elements().size());
        for (Expr bindingExpr : bindingList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingEntry)
                    || bindingEntry.elements().size() != 2) {
                return expandGenericTemplateList(listExpr, macro, bindings, renamedBindings,
                        repetitionIndex);
            }

            Expr rawNameExpr = bindingEntry.elements().get(0);
            Expr rawInitExpr = bindingEntry.elements().get(1);
            Expr expandedNameExpr = expandBindingName(rawNameExpr, macro, bindings,
                    renamedBindings, bodyRenames, repetitionIndex);
            Expr expandedInitExpr = expandTemplate(rawInitExpr, macro, bindings,
                    renamedBindings, repetitionIndex);

            expandedBindings.add(new ListExpr(List.of(expandedNameExpr, expandedInitExpr),
                    bindingExpr.line(), bindingExpr.column()));
        }

        expandedElements.add(new ListExpr(List.copyOf(expandedBindings), bindingList.line(),
                bindingList.column()));
        for (int index = 2; index < elements.size(); index++) {
            expandedElements.add(expandTemplate(elements.get(index), macro, bindings,
                    bodyRenames, repetitionIndex));
        }
        return new ListExpr(List.copyOf(expandedElements), listExpr.line(), listExpr.column());
    }

    private Expr expandLambdaTemplate(ListExpr listExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.size() < 3) {
            return expandGenericTemplateList(listExpr, macro, bindings, renamedBindings,
                    repetitionIndex);
        }

        Map<String, String> bodyRenames = new HashMap<>(renamedBindings);
        List<Expr> expandedElements = new ArrayList<>(elements.size());
        expandedElements.add(expandTemplate(elements.get(0), macro, bindings, renamedBindings,
                repetitionIndex));
        expandedElements.add(expandLambdaParameters(elements.get(1), macro, bindings,
                renamedBindings, bodyRenames, repetitionIndex));
        for (int index = 2; index < elements.size(); index++) {
            expandedElements.add(expandTemplate(elements.get(index), macro, bindings,
                    bodyRenames, repetitionIndex));
        }
        return new ListExpr(List.copyOf(expandedElements), listExpr.line(), listExpr.column());
    }

    private Expr expandLambdaParameters(Expr parametersExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Map<String, String> bodyRenames, Integer repetitionIndex) throws EvalError {
        if (parametersExpr instanceof SymbolExpr symbolExpr) {
            return expandBindingName(symbolExpr, macro, bindings, renamedBindings, bodyRenames,
                    repetitionIndex);
        }
        if (!(parametersExpr instanceof ListExpr parameterList)) {
            return expandTemplate(parametersExpr, macro, bindings, renamedBindings,
                    repetitionIndex);
        }

        List<Expr> expandedParameters = new ArrayList<>(parameterList.elements().size());
        for (Expr parameterExpr : parameterList.elements()) {
            if (".".equals(symbolName(parameterExpr))) {
                expandedParameters.add(parameterExpr);
                continue;
            }
            expandedParameters.add(expandBindingName(parameterExpr, macro, bindings,
                    renamedBindings, bodyRenames, repetitionIndex));
        }
        return new ListExpr(List.copyOf(expandedParameters), parameterList.line(),
                parameterList.column());
    }

    private Expr expandBindingName(Expr rawNameExpr, MacroValue macro,
            MatchBindings bindings, Map<String, String> renamedBindings,
            Map<String, String> bodyRenames, Integer repetitionIndex) throws EvalError {
        if (!(rawNameExpr instanceof SymbolExpr symbolExpr)) {
            return expandTemplate(rawNameExpr, macro, bindings, renamedBindings, repetitionIndex);
        }

        if (!isIntroducedBindingIdentifier(symbolExpr.name(), bindings)) {
            Expr expanded = expandTemplate(symbolExpr, macro, bindings, renamedBindings,
                    repetitionIndex);
            if (expanded instanceof SymbolExpr) {
                return expanded;
            }
            throw new EvalError("binding name must expand to a symbol");
        }

        String generatedName = nextGeneratedName(symbolExpr.name());
        bodyRenames.put(symbolExpr.name(), generatedName);
        return new SymbolExpr(generatedName, symbolExpr.line(), symbolExpr.column());
    }

    private int templateRepeatCount(Expr template, MatchBindings bindings) throws EvalError {
        Set<String> repeatedVariables = new HashSet<>();
        collectRepeatedTemplateVariables(template, bindings, repeatedVariables);
        if (repeatedVariables.isEmpty()) {
            throw new EvalError("template ellipsis must include a repeated pattern variable");
        }

        int repeatCount = -1;
        for (String variable : repeatedVariables) {
            int variableCount = bindings.repeated(variable).size();
            if (repeatCount == -1) {
                repeatCount = variableCount;
            } else if (repeatCount != variableCount) {
                throw new EvalError("template ellipsis variables must repeat in lockstep");
            }
        }
        return repeatCount;
    }

    private void collectRepeatedTemplateVariables(Expr template, MatchBindings bindings,
            Set<String> repeatedVariables) {
        String name = symbolName(template);
        if (name != null) {
            if (bindings.repeated(name) != null) {
                repeatedVariables.add(name);
            }
            return;
        }
        if (template instanceof ListExpr listExpr) {
            for (Expr element : listExpr.elements()) {
                if (!isEllipsis(element)) {
                    collectRepeatedTemplateVariables(element, bindings, repeatedVariables);
                }
            }
        }
    }

    private boolean isIntroducedBindingIdentifier(String name, MatchBindings bindings) {
        return !bindings.isPatternVariable(name)
                && !"...".equals(name)
                && !".".equals(name)
                && !isSyntaxKeyword(name);
    }

    private boolean isSyntaxKeyword(String name) {
        return switch (name) {
            case "define", "define-syntax", "set!", "if", "quote", "lambda",
                    "and", "or", "begin", "let", "cond", "define-record-type",
                    "syntax-rules", "else" -> true;
            default -> false;
        };
    }

    private boolean isEllipsis(Expr expr) {
        return "...".equals(symbolName(expr));
    }

    private String symbolName(Expr expr) {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        return null;
    }

    private String captureDefinitionSiteName(String originalName, Cell cell) {
        String capturedName = nextGeneratedName("captured$" + originalName);
        capturedBindings.put(capturedName, cell);
        return capturedName;
    }

    private String nextGeneratedName(String seed) {
        generatedNameCounter++;
        return "__macro$" + generatedNameCounter + "$" + seed;
    }

    private void requireExactArgs(String name, List<?> arguments, int exact) throws EvalError {
        if (arguments.size() != exact) {
            throw new EvalError(name + " expected exactly " + exact + " argument(s)");
        }
    }

    private boolean sameSyntax(Expr left, Expr right) {
        return switch (left) {
            case IntExpr leftInt -> right instanceof IntExpr rightInt
                    && leftInt.value() == rightInt.value();
            case RationalExpr leftRational -> right instanceof RationalExpr rightRational
                    && leftRational.numerator() == rightRational.numerator()
                    && leftRational.denominator() == rightRational.denominator();
            case InexactExpr leftInexact -> right instanceof InexactExpr rightInexact
                    && Double.compare(leftInexact.value(), rightInexact.value()) == 0;
            case BoolExpr leftBool -> right instanceof BoolExpr rightBool
                    && leftBool.value() == rightBool.value();
            case CharExpr leftChar -> right instanceof CharExpr rightChar
                    && leftChar.value() == rightChar.value();
            case StringExpr leftString -> right instanceof StringExpr rightString
                    && leftString.value().equals(rightString.value());
            case SymbolExpr leftSymbol -> right instanceof SymbolExpr rightSymbol
                    && leftSymbol.name().equals(rightSymbol.name());
            case ListExpr leftList -> right instanceof ListExpr rightList
                    && sameSyntax(leftList.elements(), rightList.elements());
        };
    }

    private boolean sameSyntax(List<Expr> left, List<Expr> right) {
        if (left.size() != right.size()) {
            return false;
        }
        for (int index = 0; index < left.size(); index++) {
            if (!sameSyntax(left.get(index), right.get(index))) {
                return false;
            }
        }
        return true;
    }

    private record MacroValue(String name, Set<String> literals, List<MacroRule> rules,
            Env definitionEnv, boolean syntaxCase) {
    }

    private record MacroRule(Expr pattern, Expr guard, Expr result) {
    }

    private sealed interface TransformerResult permits TransformerDatum, TransformerSyntax {
    }

    private record TransformerDatum(Value value) implements TransformerResult {
    }

    private record TransformerSyntax(Expr expr) implements TransformerResult {
    }

    private final class MatchBindings {
        private final Map<String, Expr> singleBindings = new HashMap<>();
        private final Map<String, List<Expr>> repeatedBindings = new HashMap<>();

        private MatchBindings() {
        }

        private MatchBindings(Map<String, Expr> singleBindings,
                Map<String, List<Expr>> repeatedBindings) {
            this.singleBindings.putAll(singleBindings);
            for (Map.Entry<String, List<Expr>> entry : repeatedBindings.entrySet()) {
                this.repeatedBindings.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
        }

        private boolean addSingle(String name, Expr value) {
            Expr existing = singleBindings.get(name);
            if (existing == null) {
                singleBindings.put(name, value);
                return true;
            }
            return sameSyntax(existing, value);
        }

        private void ensureRepeated(Set<String> names) {
            for (String name : names) {
                repeatedBindings.computeIfAbsent(name, ignored -> new ArrayList<>());
            }
        }

        private void addRepeated(String name, Expr value) {
            repeatedBindings.computeIfAbsent(name, ignored -> new ArrayList<>()).add(value);
        }

        private Expr single(String name) {
            return singleBindings.get(name);
        }

        private void putSingle(String name, Expr value) {
            singleBindings.put(name, value);
            repeatedBindings.remove(name);
        }

        private List<Expr> repeated(String name) {
            return repeatedBindings.get(name);
        }

        private boolean isPatternVariable(String name) {
            return singleBindings.containsKey(name) || repeatedBindings.containsKey(name);
        }

        private MatchBindings copy() {
            return new MatchBindings(singleBindings, repeatedBindings);
        }
    }
}
