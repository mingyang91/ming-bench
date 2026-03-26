package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class SyntaxRulesMacro implements MacroTransformer {
    private static final Set<String> CORE_SYNTAX = SyntaxMatcher.CORE_SYNTAX;

    private final long macroId;
    private final String name;
    private final Set<String> literalIdentifiers;
    private final List<SyntaxRule> rules;
    private final Environment definitionEnvironment;
    private final Map<String, String> identifierAliases = new HashMap<>();

    SyntaxRulesMacro(
            String name,
            Set<String> literalIdentifiers,
            List<SyntaxRule> rules,
            Environment definitionEnvironment,
            long macroId
    ) {
        this.macroId = macroId;
        this.name = name;
        this.literalIdentifiers = literalIdentifiers;
        this.rules = rules;
        this.definitionEnvironment = definitionEnvironment;
    }

    @Override
    public SchemeExpression expand(Evaluator evaluator, ListExpression invocation) throws EvalError {
        for (SyntaxRule rule : rules) {
            Map<String, PatternBinding> bindings = matchRule(rule.pattern(), invocation);
            if (bindings != null) {
                return expandTemplate(rule.template(), bindings, List.of());
            }
        }
        throw new EvalError(invocation.position(), name + ": no matching syntax-rules pattern");
    }

    private Map<String, PatternBinding> matchRule(SchemeExpression pattern, ListExpression invocation) {
        if (!(pattern instanceof ListExpression patternList)) {
            return null;
        }

        List<SchemeExpression> patternElements = patternList.elements();
        List<SchemeExpression> invocationElements = invocation.elements();
        if (patternElements.isEmpty() || invocationElements.isEmpty()) {
            return null;
        }
        return matchSequence(
                patternElements.subList(1, patternElements.size()),
                0,
                invocationElements.subList(1, invocationElements.size()),
                0
        );
    }

    private Map<String, PatternBinding> matchPattern(SchemeExpression pattern, SchemeExpression input) {
        if (isQuotedForm(pattern)) {
            if (expressionsEqual(pattern, input)) {
                return Map.of();
            }
            return null;
        }

        if (pattern instanceof LiteralExpression) {
            if (expressionsEqual(pattern, input)) {
                return Map.of();
            }
            return null;
        }

        if (pattern instanceof SymbolExpression symbol) {
            return matchSymbol(symbol, input);
        }

        if (!(input instanceof ListExpression inputList)) {
            return null;
        }

        List<SchemeExpression> patternElements = ((ListExpression) pattern).elements();
        List<SchemeExpression> inputElements = inputList.elements();
        return matchSequence(patternElements, 0, inputElements, 0);
    }

    private Map<String, PatternBinding> matchSymbol(SymbolExpression pattern, SchemeExpression input) {
        String symbolName = pattern.name();
        if ("...".equals(symbolName)) {
            return null;
        }
        if ("_".equals(symbolName)) {
            return Map.of();
        }
        if (literalIdentifiers.contains(symbolName)) {
            if (input instanceof SymbolExpression symbol && symbolName.equals(symbol.name())) {
                return Map.of();
            }
            return null;
        }
        return Map.of(symbolName, new SinglePatternBinding(input));
    }

    private Map<String, PatternBinding> matchSequence(
            List<SchemeExpression> patterns,
            int patternIndex,
            List<SchemeExpression> inputs,
            int inputIndex
    ) {
        if (patternIndex >= patterns.size()) {
            if (inputIndex == inputs.size()) {
                return Map.of();
            }
            return null;
        }

        SchemeExpression pattern = patterns.get(patternIndex);
        if (patternIndex + 1 < patterns.size() && isEllipsis(patterns.get(patternIndex + 1))) {
            Set<String> repeatedVariables = collectPatternVariables(pattern);
            int maxCount = inputs.size() - inputIndex;
            for (int count = 0; count <= maxCount; count++) {
                List<Map<String, PatternBinding>> iterationMatches = new ArrayList<>(count);
                boolean matched = true;
                for (int offset = 0; offset < count; offset++) {
                    Map<String, PatternBinding> iterationMatch =
                            matchPattern(pattern, inputs.get(inputIndex + offset));
                    if (iterationMatch == null) {
                        matched = false;
                        break;
                    }
                    iterationMatches.add(iterationMatch);
                }

                if (!matched) {
                    continue;
                }

                Map<String, PatternBinding> repeatedBindings =
                        aggregateRepeatedBindings(repeatedVariables, iterationMatches);
                Map<String, PatternBinding> remainder = matchSequence(
                        patterns,
                        patternIndex + 2,
                        inputs,
                        inputIndex + count
                );
                if (remainder == null) {
                    continue;
                }

                Map<String, PatternBinding> merged = mergeBindings(repeatedBindings, remainder);
                if (merged != null) {
                    return merged;
                }
            }
            return null;
        }

        if (inputIndex >= inputs.size()) {
            return null;
        }

        Map<String, PatternBinding> currentMatch = matchPattern(pattern, inputs.get(inputIndex));
        if (currentMatch == null) {
            return null;
        }

        Map<String, PatternBinding> remainder =
                matchSequence(patterns, patternIndex + 1, inputs, inputIndex + 1);
        if (remainder == null) {
            return null;
        }
        return mergeBindings(currentMatch, remainder);
    }

    private Map<String, PatternBinding> aggregateRepeatedBindings(
            Set<String> repeatedVariables,
            List<Map<String, PatternBinding>> iterationMatches
    ) {
        Map<String, PatternBinding> bindings = new HashMap<>();
        for (String variable : repeatedVariables) {
            List<PatternBinding> items = new ArrayList<>(iterationMatches.size());
            for (Map<String, PatternBinding> iterationMatch : iterationMatches) {
                items.add(iterationMatch.get(variable));
            }
            bindings.put(variable, new RepeatedPatternBinding(List.copyOf(items)));
        }
        return bindings;
    }

    private Map<String, PatternBinding> mergeBindings(
            Map<String, PatternBinding> left,
            Map<String, PatternBinding> right
    ) {
        Map<String, PatternBinding> merged = new HashMap<>(left);
        for (Map.Entry<String, PatternBinding> entry : right.entrySet()) {
            PatternBinding existing = merged.get(entry.getKey());
            if (existing != null && !bindingsEqual(existing, entry.getValue())) {
                return null;
            }
            merged.put(entry.getKey(), entry.getValue());
        }
        return merged;
    }

    private Set<String> collectPatternVariables(SchemeExpression pattern) {
        Set<String> variables = new HashSet<>();
        collectPatternVariables(pattern, variables);
        return variables;
    }

    private void collectPatternVariables(SchemeExpression pattern, Set<String> variables) {
        if (isQuotedForm(pattern) || pattern instanceof LiteralExpression) {
            return;
        }

        if (pattern instanceof SymbolExpression symbol) {
            String name = symbol.name();
            if (!literalIdentifiers.contains(name) && !"_".equals(name) && !"...".equals(name)) {
                variables.add(name);
            }
            return;
        }

        for (SchemeExpression element : ((ListExpression) pattern).elements()) {
            collectPatternVariables(element, variables);
        }
    }

    private SchemeExpression expandTemplate(
            SchemeExpression template,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath
    ) throws EvalError {
        if (template instanceof LiteralExpression) {
            return template;
        }
        if (template instanceof SymbolExpression symbol) {
            return expandSymbol(symbol, bindings, repetitionPath);
        }

        if (isQuotedForm(template)) {
            return template;
        }

        ListExpression list = (ListExpression) template;
        return new ListExpression(
                List.copyOf(expandTemplateSequence(list.elements(), bindings, repetitionPath)),
                list.position()
        );
    }

    private List<SchemeExpression> expandTemplateSequence(
            List<SchemeExpression> templates,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath
    ) throws EvalError {
        List<SchemeExpression> expanded = new ArrayList<>();
        for (int index = 0; index < templates.size(); index++) {
            SchemeExpression template = templates.get(index);
            if (index + 1 < templates.size() && isEllipsis(templates.get(index + 1))) {
                int repeatCount = determineRepeatCount(template, bindings, repetitionPath);
                for (int repeatIndex = 0; repeatIndex < repeatCount; repeatIndex++) {
                    expanded.add(expandTemplate(
                            template,
                            bindings,
                            appendIndex(repetitionPath, repeatIndex)
                    ));
                }
                index++;
                continue;
            }
            expanded.add(expandTemplate(template, bindings, repetitionPath));
        }
        return expanded;
    }

    private SchemeExpression expandSymbol(
            SymbolExpression symbol,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath
    ) throws EvalError {
        PatternBinding binding = bindings.get(symbol.name());
        if (binding != null) {
            return resolveBinding(binding, repetitionPath, symbol.name());
        }

        String symbolName = symbol.name();
        if ("...".equals(symbolName) || CORE_SYNTAX.contains(symbolName)) {
            return symbol;
        }
        return new SymbolExpression(aliasFor(symbolName), symbol.position());
    }

    private int determineRepeatCount(
            SchemeExpression template,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath
    ) throws EvalError {
        Set<String> templateVariables = new HashSet<>();
        collectTemplateVariables(template, bindings.keySet(), templateVariables);

        int repeatCount = -1;
        for (String variable : templateVariables) {
            int variableRepeatCount = repeatCount(bindings.get(variable), repetitionPath);
            if (variableRepeatCount < 0) {
                continue;
            }
            if (repeatCount < 0) {
                repeatCount = variableRepeatCount;
                continue;
            }
            if (repeatCount != variableRepeatCount) {
                throw new EvalError("syntax-rules: mismatched ellipsis lengths");
            }
        }

        if (repeatCount < 0) {
            throw new EvalError("syntax-rules: ellipsis template has no repeated pattern variables");
        }
        return repeatCount;
    }

    private void collectTemplateVariables(
            SchemeExpression template,
            Set<String> boundVariables,
            Set<String> variables
    ) {
        if (isQuotedForm(template) || template instanceof LiteralExpression) {
            return;
        }

        if (template instanceof SymbolExpression symbol) {
            if (boundVariables.contains(symbol.name())) {
                variables.add(symbol.name());
            }
            return;
        }

        for (SchemeExpression element : ((ListExpression) template).elements()) {
            collectTemplateVariables(element, boundVariables, variables);
        }
    }

    private int repeatCount(PatternBinding binding, List<Integer> repetitionPath) throws EvalError {
        PatternBinding current = descend(binding, repetitionPath);
        if (current instanceof RepeatedPatternBinding repeated) {
            return repeated.items().size();
        }
        return -1;
    }

    private SchemeExpression resolveBinding(
            PatternBinding binding,
            List<Integer> repetitionPath,
            String name
    ) throws EvalError {
        PatternBinding current = descend(binding, repetitionPath);
        if (current instanceof SinglePatternBinding single) {
            return single.expression();
        }
        throw new EvalError("syntax-rules: pattern variable " + name + " used outside ellipsis");
    }

    private PatternBinding descend(PatternBinding binding, List<Integer> repetitionPath) throws EvalError {
        PatternBinding current = binding;
        for (Integer index : repetitionPath) {
            if (!(current instanceof RepeatedPatternBinding repeated)) {
                throw new EvalError("syntax-rules: invalid ellipsis nesting");
            }
            if (index < 0 || index >= repeated.items().size()) {
                throw new EvalError("syntax-rules: ellipsis index out of bounds");
            }
            current = repeated.items().get(index);
        }
        return current;
    }

    private String aliasFor(String name) {
        return identifierAliases.computeIfAbsent(name, key -> {
            String alias = "__macro$" + macroId + "$" + key;
            definitionEnvironment.defineAlias(alias, key);
            return alias;
        });
    }

    private List<Integer> appendIndex(List<Integer> repetitionPath, int index) {
        List<Integer> extendedPath = new ArrayList<>(repetitionPath.size() + 1);
        extendedPath.addAll(repetitionPath);
        extendedPath.add(index);
        return List.copyOf(extendedPath);
    }

    private boolean bindingsEqual(PatternBinding left, PatternBinding right) {
        if (left instanceof SinglePatternBinding leftSingle && right instanceof SinglePatternBinding rightSingle) {
            return expressionsEqual(leftSingle.expression(), rightSingle.expression());
        }
        if (left instanceof RepeatedPatternBinding leftRepeated
                && right instanceof RepeatedPatternBinding rightRepeated) {
            if (leftRepeated.items().size() != rightRepeated.items().size()) {
                return false;
            }
            for (int index = 0; index < leftRepeated.items().size(); index++) {
                if (!bindingsEqual(leftRepeated.items().get(index), rightRepeated.items().get(index))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private boolean expressionsEqual(SchemeExpression left, SchemeExpression right) {
        if (left instanceof LiteralExpression leftLiteral && right instanceof LiteralExpression rightLiteral) {
            return literalValuesEqual(leftLiteral.value(), rightLiteral.value());
        }
        if (left instanceof SymbolExpression leftSymbol && right instanceof SymbolExpression rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof ListExpression leftList && right instanceof ListExpression rightList) {
            if (leftList.elements().size() != rightList.elements().size()) {
                return false;
            }
            for (int index = 0; index < leftList.elements().size(); index++) {
                if (!expressionsEqual(leftList.elements().get(index), rightList.elements().get(index))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private boolean literalValuesEqual(SchemeValue left, SchemeValue right) {
        if (left == right) {
            return true;
        }
        if (left == null || right == null || left.getClass() != right.getClass()) {
            return false;
        }
        if (left instanceof IntValue leftInt && right instanceof IntValue rightInt) {
            return leftInt.value() == rightInt.value();
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    private boolean isQuotedForm(SchemeExpression expression) {
        if (!(expression instanceof ListExpression list) || list.elements().size() != 2) {
            return false;
        }
        return list.elements().getFirst() instanceof SymbolExpression symbol
                && "quote".equals(symbol.name());
    }

    private boolean isEllipsis(SchemeExpression expression) {
        return expression instanceof SymbolExpression symbol && "...".equals(symbol.name());
    }

    record SyntaxRule(SchemeExpression pattern, SchemeExpression template) {
    }

    private sealed interface PatternBinding permits SinglePatternBinding, RepeatedPatternBinding {
    }

    private record SinglePatternBinding(SchemeExpression expression) implements PatternBinding {
    }

    private record RepeatedPatternBinding(List<PatternBinding> items) implements PatternBinding {
    }
}
