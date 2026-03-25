package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

interface SyntaxMacro {
    Expr expand(ListExpr invocation) throws EvalError;
}

final class SyntaxRulesMacro implements SyntaxMacro {
    private static long freshCounter = 0;

    private final String name;
    private final Set<String> literals;
    private final List<Rule> rules;
    private final Environment definitionEnvironment;

    private SyntaxRulesMacro(String name, Set<String> literals, List<Rule> rules,
                             Environment definitionEnvironment) {
        this.name = name;
        this.literals = literals;
        this.rules = rules;
        this.definitionEnvironment = definitionEnvironment;
    }

    static SyntaxRulesMacro fromDefinition(String name, Expr transformer,
                                           Environment definitionEnvironment)
            throws EvalError {
        if (!(transformer instanceof ListExpr transformerList)) {
            throw error("'define-syntax' expects a syntax-rules transformer", transformer.pos());
        }

        List<Expr> transformerElements = transformerList.elements();
        if (transformerElements.size() < 3) {
            throw error("'syntax-rules' expects literals and at least one rule",
                    transformerList.pos());
        }
        if (!(transformerElements.getFirst() instanceof SymbolExpr head)
                || !"syntax-rules".equals(head.name())) {
            throw error("'define-syntax' expects a syntax-rules transformer",
                    transformerList.pos());
        }
        if (!(transformerElements.get(1) instanceof ListExpr literalList)) {
            throw error("'syntax-rules' literals must be a list", transformerElements.get(1).pos());
        }

        Set<String> literals = new HashSet<>();
        for (Expr literalExpr : literalList.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw error("'syntax-rules' literals must be identifiers", literalExpr.pos());
            }
            literals.add(literalSymbol.name());
        }

        List<Rule> rules = new ArrayList<>();
        for (int i = 2; i < transformerElements.size(); i++) {
            Expr ruleExpr = transformerElements.get(i);
            if (!(ruleExpr instanceof ListExpr ruleList)
                    || ruleList.elements().size() != 2) {
                throw error("'syntax-rules' rules must be (pattern template) pairs",
                        ruleExpr.pos());
            }

            Expr pattern = ruleList.elements().get(0);
            if (!(pattern instanceof ListExpr patternList)
                    || patternList.elements().isEmpty()) {
                throw error("macro pattern must be a non-empty list", pattern.pos());
            }
            if (!(patternList.elements().getFirst() instanceof SymbolExpr patternHead)
                    || !name.equals(patternHead.name())) {
                throw error("macro pattern must start with " + name, pattern.pos());
            }

            rules.add(new Rule(pattern, ruleList.elements().get(1)));
        }

        return new SyntaxRulesMacro(name, Set.copyOf(literals), rules, definitionEnvironment);
    }

    @Override
    public Expr expand(ListExpr invocation) throws EvalError {
        for (Rule rule : rules) {
            MatchBindings bindings = new MatchBindings();
            if (match(rule.pattern(), invocation, bindings)) {
                return instantiate(rule.template(), bindings, new HashMap<>(), null);
            }
        }
        throw error("no matching syntax-rules clause for " + name, invocation.pos());
    }

    private boolean match(Expr pattern, Expr expr, MatchBindings bindings) {
        if (pattern instanceof SymbolExpr symbolPattern) {
            String identifier = symbolPattern.name();
            if ("...".equals(identifier)) {
                return expr instanceof SymbolExpr symbolExpr
                        && "...".equals(symbolExpr.name());
            }
            if ("_".equals(identifier)) {
                return true;
            }
            if (isLiteralIdentifier(identifier)) {
                return expr instanceof SymbolExpr symbolExpr
                        && identifier.equals(symbolExpr.name());
            }
            return bindings.bindSingle(identifier, expr);
        }

        if (pattern instanceof ListExpr listPattern) {
            if (!(expr instanceof ListExpr listExpr)) {
                return false;
            }
            return matchList(listPattern.elements(), listExpr.elements(), 0, 0, bindings);
        }

        return syntaxEquals(pattern, expr);
    }

    private boolean matchList(List<Expr> patterns, List<Expr> expressions,
                              int patternIndex, int expressionIndex, MatchBindings bindings) {
        if (patternIndex == patterns.size()) {
            return expressionIndex == expressions.size();
        }
        if (expressionIndex > expressions.size()) {
            return false;
        }

        if (patternIndex + 1 < patterns.size() && isEllipsis(patterns.get(patternIndex + 1))) {
            int minRemaining = minRequiredExpressions(patterns, patternIndex + 2);
            int maxRepetitions = expressions.size() - expressionIndex - minRemaining;
            if (maxRepetitions < 0) {
                return false;
            }

            for (int repetitions = 0; repetitions <= maxRepetitions; repetitions++) {
                MatchBindings candidate = bindings.copy();
                initializeRepeatedBindings(patterns.get(patternIndex), candidate);
                boolean matched = true;
                for (int i = 0; i < repetitions; i++) {
                    if (!matchRepeated(patterns.get(patternIndex),
                            expressions.get(expressionIndex + i), candidate)) {
                        matched = false;
                        break;
                    }
                }
                if (matched && matchList(patterns, expressions, patternIndex + 2,
                        expressionIndex + repetitions, candidate)) {
                    bindings.replaceWith(candidate);
                    return true;
                }
            }
            return false;
        }

        if (expressionIndex >= expressions.size()) {
            return false;
        }
        if (!match(patterns.get(patternIndex), expressions.get(expressionIndex), bindings)) {
            return false;
        }
        return matchList(patterns, expressions, patternIndex + 1, expressionIndex + 1, bindings);
    }

    private boolean matchRepeated(Expr pattern, Expr expr,
                                  MatchBindings bindings) {
        MatchBindings iterationBindings = new MatchBindings();
        if (!match(pattern, expr, iterationBindings)) {
            return false;
        }
        bindings.appendRepeated(iterationBindings);
        return true;
    }

    private int minRequiredExpressions(List<Expr> patterns, int startIndex) {
        int required = 0;
        for (int i = startIndex; i < patterns.size(); i++) {
            if (i + 1 < patterns.size() && isEllipsis(patterns.get(i + 1))) {
                i++;
                continue;
            }
            required++;
        }
        return required;
    }

    private void initializeRepeatedBindings(Expr pattern, MatchBindings bindings) {
        Set<String> variables = new HashSet<>();
        collectPatternVariables(pattern, variables);
        for (String variable : variables) {
            bindings.ensureRepeated(variable);
        }
    }

    private void collectPatternVariables(Expr pattern, Set<String> variables) {
        if (pattern instanceof SymbolExpr symbolPattern) {
            String identifier = symbolPattern.name();
            if (!"...".equals(identifier) && !"_".equals(identifier)
                    && !isLiteralIdentifier(identifier)) {
                variables.add(identifier);
            }
            return;
        }
        if (pattern instanceof ListExpr listPattern) {
            for (Expr element : listPattern.elements()) {
                if (isEllipsis(element)) {
                    continue;
                }
                collectPatternVariables(element, variables);
            }
        }
    }

    private Expr instantiate(Expr template, MatchBindings bindings, Map<String, String> renames,
                             Integer repetitionIndex)
            throws EvalError {
        if (template instanceof SymbolExpr symbolTemplate) {
            return instantiateSymbol(symbolTemplate, bindings, renames, repetitionIndex);
        }
        if (template instanceof ListExpr listTemplate) {
            return instantiateList(listTemplate, bindings, renames, repetitionIndex);
        }
        return template;
    }

    private Expr instantiateSymbol(SymbolExpr symbolTemplate, MatchBindings bindings,
                                   Map<String, String> renames,
                                             Integer repetitionIndex) throws EvalError {
        String name = symbolTemplate.name();
        if (bindings.hasSingle(name)) {
            return bindings.single(name);
        }
        if (bindings.hasRepeated(name)) {
            if (repetitionIndex == null) {
                throw error("repeated pattern variable used outside ellipsis",
                        symbolTemplate.pos());
            }
            return bindings.repeated(name, repetitionIndex, symbolTemplate.pos());
        }
        if (renames.containsKey(name)) {
            return new SymbolExpr(renames.get(name), symbolTemplate.pos());
        }
        return new SymbolExpr(name, symbolTemplate.pos(), definitionEnvironment);
    }

    private Expr instantiateList(ListExpr template, MatchBindings bindings,
                                 Map<String, String> renames, Integer repetitionIndex)
            throws EvalError {
        if (isBindingForm(template, "let", bindings)) {
            return instantiateLet(template, bindings, renames, repetitionIndex);
        }
        if (isBindingForm(template, "lambda", bindings)) {
            return instantiateLambda(template, bindings, renames, repetitionIndex);
        }

        List<Expr> result = new ArrayList<>();
        List<Expr> elements = template.elements();
        for (int i = 0; i < elements.size(); i++) {
            Expr element = elements.get(i);
            if (i + 1 < elements.size() && isEllipsis(elements.get(i + 1))) {
                int count = repetitionCount(element, bindings);
                for (int repetition = 0; repetition < count; repetition++) {
                    result.add(instantiate(element, bindings, renames, repetition));
                }
                i++;
                continue;
            }
            result.add(instantiate(element, bindings, renames, repetitionIndex));
        }
        return new ListExpr(result, template.pos());
    }

    private Expr instantiateLet(ListExpr template, MatchBindings bindings,
                                Map<String, String> renames, Integer repetitionIndex)
            throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3 || !(elements.get(1) instanceof ListExpr bindingList)) {
            return instantiatePlainList(template, bindings, renames, repetitionIndex);
        }

        List<Expr> rewrittenElements = new ArrayList<>(elements.size());
        rewrittenElements.add(instantiate(elements.getFirst(), bindings, renames, repetitionIndex));

        Map<String, String> bodyRenames = new HashMap<>(renames);
        List<Expr> rewrittenBindings = new ArrayList<>(bindingList.elements().size());
        for (Expr bindingExpr : bindingList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingPair)
                    || bindingPair.elements().isEmpty()) {
                rewrittenBindings.add(instantiate(bindingExpr, bindings, renames,
                        repetitionIndex));
                continue;
            }

            List<Expr> bindingElements = bindingPair.elements();
            List<Expr> rewrittenBinding = new ArrayList<>(bindingElements.size());
            rewrittenBinding.add(instantiateBindingIdentifier(bindingElements.getFirst(),
                    bindings, renames, bodyRenames, repetitionIndex));
            for (int i = 1; i < bindingElements.size(); i++) {
                rewrittenBinding.add(instantiate(bindingElements.get(i), bindings, renames,
                        repetitionIndex));
            }
            rewrittenBindings.add(new ListExpr(rewrittenBinding, bindingPair.pos()));
        }

        rewrittenElements.add(new ListExpr(rewrittenBindings, bindingList.pos()));
        for (int i = 2; i < elements.size(); i++) {
            rewrittenElements.add(instantiate(elements.get(i), bindings, bodyRenames,
                    repetitionIndex));
        }
        return new ListExpr(rewrittenElements, template.pos());
    }

    private Expr instantiateLambda(ListExpr template, MatchBindings bindings,
                                   Map<String, String> renames, Integer repetitionIndex)
            throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3) {
            return instantiatePlainList(template, bindings, renames, repetitionIndex);
        }

        List<Expr> rewrittenElements = new ArrayList<>(elements.size());
        rewrittenElements.add(instantiate(elements.getFirst(), bindings, renames, repetitionIndex));

        Map<String, String> bodyRenames = new HashMap<>(renames);
        rewrittenElements.add(instantiateLambdaParameters(elements.get(1), bindings, renames,
                bodyRenames, repetitionIndex));
        for (int i = 2; i < elements.size(); i++) {
            rewrittenElements.add(instantiate(elements.get(i), bindings, bodyRenames,
                    repetitionIndex));
        }
        return new ListExpr(rewrittenElements, template.pos());
    }

    private Expr instantiateLambdaParameters(Expr parameterExpr, MatchBindings bindings,
                                                       Map<String, String> renames,
                                                       Map<String, String> bodyRenames,
                                                       Integer repetitionIndex)
            throws EvalError {
        if (parameterExpr instanceof SymbolExpr symbolParameter) {
            return instantiateBindingIdentifier(symbolParameter, bindings, renames, bodyRenames,
                    repetitionIndex);
        }
        if (!(parameterExpr instanceof ListExpr parameterList)) {
            return instantiate(parameterExpr, bindings, renames, repetitionIndex);
        }

        List<Expr> rewrittenParameters = new ArrayList<>(parameterList.elements().size());
        for (Expr parameter : parameterList.elements()) {
            if (parameter instanceof SymbolExpr parameterSymbol
                    && ".".equals(parameterSymbol.name())) {
                rewrittenParameters.add(new SymbolExpr(".", parameter.pos()));
                continue;
            }
            rewrittenParameters.add(instantiateBindingIdentifier(parameter, bindings, renames,
                    bodyRenames, repetitionIndex));
        }
        return new ListExpr(rewrittenParameters, parameterList.pos());
    }

    private Expr instantiateBindingIdentifier(Expr bindingExpr, MatchBindings bindings,
                                                        Map<String, String> renames,
                                                        Map<String, String> bodyRenames,
                                                        Integer repetitionIndex)
            throws EvalError {
        if (!(bindingExpr instanceof SymbolExpr bindingSymbol)) {
            return instantiate(bindingExpr, bindings, renames, repetitionIndex);
        }

        String name = bindingSymbol.name();
        if (bindings.hasSingle(name) || bindings.hasRepeated(name)) {
            return instantiate(bindingSymbol, bindings, renames, repetitionIndex);
        }

        String freshName = freshIdentifier(name);
        bodyRenames.put(name, freshName);
        return new SymbolExpr(freshName, bindingSymbol.pos());
    }

    private Expr instantiatePlainList(ListExpr template, MatchBindings bindings,
                                      Map<String, String> renames, Integer repetitionIndex)
            throws EvalError {
        List<Expr> rewrittenElements = new ArrayList<>(template.elements().size());
        for (Expr element : template.elements()) {
            rewrittenElements.add(instantiate(element, bindings, renames, repetitionIndex));
        }
        return new ListExpr(rewrittenElements, template.pos());
    }

    private int repetitionCount(Expr template, MatchBindings bindings) throws EvalError {
        Set<String> repeatedVariables = new HashSet<>();
        collectRepeatedVariables(template, bindings, repeatedVariables);
        Integer count = null;
        for (String variable : repeatedVariables) {
            int variableCount = bindings.repeatedCount(variable);
            if (count == null) {
                count = variableCount;
                continue;
            }
            if (count != variableCount) {
                throw error("mismatched ellipsis repetition counts", template.pos());
            }
        }
        if (count == null) {
            throw error("template ellipsis must reference a repeated pattern variable",
                    template.pos());
        }
        return count;
    }

    private void collectRepeatedVariables(Expr template, MatchBindings bindings,
                                          Set<String> repeatedVariables) {
        if (template instanceof SymbolExpr symbolTemplate) {
            if (bindings.hasRepeated(symbolTemplate.name())) {
                repeatedVariables.add(symbolTemplate.name());
            }
            return;
        }
        if (template instanceof ListExpr listTemplate) {
            for (int i = 0; i < listTemplate.elements().size(); i++) {
                Expr element = listTemplate.elements().get(i);
                if (isEllipsis(element)) {
                    continue;
                }
                collectRepeatedVariables(element, bindings, repeatedVariables);
            }
        }
    }

    private boolean isBindingForm(ListExpr template, String keyword, MatchBindings bindings) {
        if (template.elements().isEmpty()) {
            return false;
        }
        Expr head = template.elements().getFirst();
        if (!(head instanceof SymbolExpr symbolHead)) {
            return false;
        }
        return keyword.equals(symbolHead.name())
                && !bindings.hasSingle(symbolHead.name())
                && !bindings.hasRepeated(symbolHead.name());
    }

    private boolean isLiteralIdentifier(String identifier) {
        return name.equals(identifier) || literals.contains(identifier);
    }

    private boolean isEllipsis(Expr expr) {
        return expr instanceof SymbolExpr symbolExpr
                && "...".equals(symbolExpr.name());
    }

    private boolean syntaxEquals(Expr left, Expr right) {
        if (left.getClass() != right.getClass()) {
            return false;
        }
        if (left instanceof NumberExpr leftNumber && right instanceof NumberExpr rightNumber) {
            return leftNumber.value().equals(rightNumber.value());
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
                if (!syntaxEquals(leftList.elements().get(i), rightList.elements().get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private static synchronized String freshIdentifier(String base) {
        freshCounter++;
        return "__macro_" + freshCounter + "_" + base;
    }

    private static EvalError error(String message, SourcePos pos) {
        return new EvalError(message, pos.line(), pos.column());
    }

    private record Rule(Expr pattern, Expr template) {
    }

    private static final class MatchBindings {
        private final Map<String, Expr> singleBindings = new HashMap<>();
        private final Map<String, List<Expr>> repeatedBindings = new HashMap<>();

        private boolean bindSingle(String name, Expr expr) {
            if (repeatedBindings.containsKey(name)) {
                return false;
            }
            if (!singleBindings.containsKey(name)) {
                singleBindings.put(name, expr);
                return true;
            }
            return syntaxEquals(singleBindings.get(name), expr);
        }

        private boolean hasSingle(String name) {
            return singleBindings.containsKey(name);
        }

        private boolean hasRepeated(String name) {
            return repeatedBindings.containsKey(name);
        }

        private Expr single(String name) {
            return singleBindings.get(name);
        }

        private Expr repeated(String name, int index, SourcePos pos) throws EvalError {
            List<Expr> values = repeatedBindings.get(name);
            if (values == null || index < 0 || index >= values.size()) {
                throw error("ellipsis repetition out of range for " + name, pos);
            }
            return values.get(index);
        }

        private int repeatedCount(String name) {
            List<Expr> values = repeatedBindings.get(name);
            return values == null ? 0 : values.size();
        }

        private void ensureRepeated(String name) {
            if (!singleBindings.containsKey(name)) {
                repeatedBindings.computeIfAbsent(name, ignored -> new ArrayList<>());
            }
        }

        private void appendRepeated(MatchBindings iterationBindings) {
            for (Map.Entry<String, Expr> entry : iterationBindings.singleBindings.entrySet()) {
                repeatedBindings.computeIfAbsent(entry.getKey(), ignored -> new ArrayList<>())
                        .add(entry.getValue());
            }
            for (Map.Entry<String, List<Expr>> entry : iterationBindings.repeatedBindings.entrySet()) {
                repeatedBindings.computeIfAbsent(entry.getKey(), ignored -> new ArrayList<>())
                        .addAll(entry.getValue());
            }
        }

        private MatchBindings copy() {
            MatchBindings copy = new MatchBindings();
            copy.singleBindings.putAll(singleBindings);
            for (Map.Entry<String, List<Expr>> entry : repeatedBindings.entrySet()) {
                copy.repeatedBindings.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
            return copy;
        }

        private void replaceWith(MatchBindings candidate) {
            singleBindings.clear();
            repeatedBindings.clear();
            singleBindings.putAll(candidate.singleBindings);
            for (Map.Entry<String, List<Expr>> entry : candidate.repeatedBindings.entrySet()) {
                repeatedBindings.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
        }

        private static boolean syntaxEquals(Expr left, Expr right) {
            if (left.getClass() != right.getClass()) {
                return false;
            }
            if (left instanceof NumberExpr leftNumber && right instanceof NumberExpr rightNumber) {
                return leftNumber.value().equals(rightNumber.value());
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
                    if (!syntaxEquals(leftList.elements().get(i), rightList.elements().get(i))) {
                        return false;
                    }
                }
                return true;
            }
            return false;
        }
    }
}
