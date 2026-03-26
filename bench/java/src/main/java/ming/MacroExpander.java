package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

final class MacroExpander {
    private final Map<String, MacroValue> macros = new HashMap<>();
    private final Map<String, Cell> capturedBindings = new HashMap<>();
    private long generatedNameCounter;

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
            throw new EvalError("define-syntax expects a syntax-rules transformer");
        }

        List<Expr> transformer = transformerList.elements();
        if (transformer.size() < 3 || !"syntax-rules".equals(symbolName(transformer.get(0)))) {
            throw new EvalError("define-syntax expects a syntax-rules transformer");
        }

        Set<String> literals = parseLiteralIdentifiers(transformer.get(1));
        List<MacroRule> rules = new ArrayList<>();
        for (int index = 2; index < transformer.size(); index++) {
            Expr ruleExpr = transformer.get(index);
            if (!(ruleExpr instanceof ListExpr ruleList) || ruleList.elements().size() != 2) {
                throw new EvalError("syntax-rules clause must contain a pattern and template");
            }
            rules.add(new MacroRule(ruleList.elements().get(0), ruleList.elements().get(1)));
        }

        return new MacroValue(name, Set.copyOf(literals), List.copyOf(rules), env);
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
                return expandTemplate(rule.template(), macro, bindings, new HashMap<>(), null);
            }
        }
        throw new EvalError("macro invocation did not match any syntax-rules clause");
    }

    private boolean matchPattern(Expr pattern, Expr input, Set<String> literals,
            String macroName, MatchBindings bindings, boolean repeatedContext)
            throws EvalError {
        String patternName = symbolName(pattern);
        if (patternName != null) {
            if ("...".equals(patternName)) {
                throw new EvalError("ellipsis cannot appear by itself in a pattern");
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
            Env definitionEnv) {
    }

    private record MacroRule(Expr pattern, Expr template) {
    }

    private final class MatchBindings {
        private final Map<String, Expr> singleBindings = new HashMap<>();
        private final Map<String, List<Expr>> repeatedBindings = new HashMap<>();

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

        private List<Expr> repeated(String name) {
            return repeatedBindings.get(name);
        }

        private boolean isPatternVariable(String name) {
            return singleBindings.containsKey(name) || repeatedBindings.containsKey(name);
        }
    }
}
