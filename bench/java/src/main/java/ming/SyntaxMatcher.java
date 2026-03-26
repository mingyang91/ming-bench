package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class SyntaxMatcher {
    static final Set<String> CORE_SYNTAX = Set.of(
            "and",
            "begin",
            "case",
            "case-lambda",
            "cond",
            "define",
            "define-record-type",
            "define-syntax",
            "do",
            "else",
            "guard",
            "if",
            "lambda",
            "let",
            "let*",
            "letrec",
            "letrec*",
            "or",
            "quasiquote",
            "quote",
            "set!",
            "syntax",
            "syntax-case",
            "syntax-rules",
            "unquote",
            "unquote-splicing",
            "with-syntax",
            "."
    );

    private SyntaxMatcher() {
    }

    static Map<String, PatternBinding> matchPattern(
            SchemeExpression pattern,
            SyntaxValue input,
            Set<String> literalIdentifiers
    ) {
        if (isQuotedForm(pattern)) {
            if (expressionsEqual(pattern, input.expression())) {
                return Map.of();
            }
            return null;
        }

        if (pattern instanceof LiteralExpression) {
            if (expressionsEqual(pattern, input.expression())) {
                return Map.of();
            }
            return null;
        }

        if (pattern instanceof SymbolExpression symbol) {
            return matchSymbol(symbol, input, literalIdentifiers);
        }

        if (!(input.expression() instanceof ListExpression inputList)) {
            return null;
        }

        List<SchemeExpression> patternElements = ((ListExpression) pattern).elements();
        List<SyntaxValue> inputElements = syntaxElements(inputList.elements(), input.context());
        return matchSequence(patternElements, 0, inputElements, 0, literalIdentifiers);
    }

    static Map<String, PatternBinding> mergeBindings(
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

    static SchemeExpression expandTemplate(
            SchemeExpression template,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath,
            SyntaxContext defaultContext
    ) throws EvalError {
        if (template instanceof LiteralExpression) {
            return template;
        }
        if (template instanceof SymbolExpression symbol) {
            return expandSymbol(symbol, bindings, repetitionPath, defaultContext);
        }
        if (isQuotedForm(template)) {
            return template;
        }

        ListExpression list = (ListExpression) template;
        return new ListExpression(
                List.copyOf(expandTemplateSequence(list.elements(), bindings, repetitionPath, defaultContext)),
                list.position()
        );
    }

    static SchemeExpression contextualize(SchemeExpression expression, SyntaxContext context) {
        if (context instanceof UseSiteSyntaxContext) {
            return expression;
        }
        if (expression instanceof LiteralExpression) {
            return expression;
        }
        if (expression instanceof SymbolExpression symbol) {
            if ("...".equals(symbol.name()) || CORE_SYNTAX.contains(symbol.name())) {
                return symbol;
            }
            return ((SyntaxTemplateContext) context).aliasSymbol(symbol);
        }
        if (isQuotedForm(expression)) {
            return expression;
        }

        ListExpression list = (ListExpression) expression;
        List<SchemeExpression> elements = new ArrayList<>(list.elements().size());
        for (SchemeExpression element : list.elements()) {
            elements.add(contextualize(element, context));
        }
        return new ListExpression(List.copyOf(elements), list.position());
    }

    static Set<String> literalIdentifiers(ListExpression literalsExpression) throws EvalError {
        Set<String> literalIdentifiers = new HashSet<>();
        for (SchemeExpression literal : literalsExpression.elements()) {
            if (!(literal instanceof SymbolExpression literalSymbol)) {
                throw new EvalError("syntax-case: expected literal identifier");
            }
            literalIdentifiers.add(literalSymbol.name());
        }
        return Set.copyOf(literalIdentifiers);
    }

    private static Map<String, PatternBinding> matchSequence(
            List<SchemeExpression> patterns,
            int patternIndex,
            List<SyntaxValue> inputs,
            int inputIndex,
            Set<String> literalIdentifiers
    ) {
        if (patternIndex >= patterns.size()) {
            if (inputIndex == inputs.size()) {
                return Map.of();
            }
            return null;
        }

        SchemeExpression pattern = patterns.get(patternIndex);
        if (patternIndex + 1 < patterns.size() && isEllipsis(patterns.get(patternIndex + 1))) {
            Set<String> repeatedVariables = collectPatternVariables(pattern, literalIdentifiers);
            int maxCount = inputs.size() - inputIndex;
            for (int count = 0; count <= maxCount; count++) {
                List<Map<String, PatternBinding>> iterationMatches = new ArrayList<>(count);
                boolean matched = true;
                for (int offset = 0; offset < count; offset++) {
                    Map<String, PatternBinding> iterationMatch =
                            matchPattern(pattern, inputs.get(inputIndex + offset), literalIdentifiers);
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
                        inputIndex + count,
                        literalIdentifiers
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

        Map<String, PatternBinding> currentMatch =
                matchPattern(pattern, inputs.get(inputIndex), literalIdentifiers);
        if (currentMatch == null) {
            return null;
        }

        Map<String, PatternBinding> remainder =
                matchSequence(patterns, patternIndex + 1, inputs, inputIndex + 1, literalIdentifiers);
        if (remainder == null) {
            return null;
        }
        return mergeBindings(currentMatch, remainder);
    }

    private static Map<String, PatternBinding> matchSymbol(
            SymbolExpression pattern,
            SyntaxValue input,
            Set<String> literalIdentifiers
    ) {
        String symbolName = pattern.name();
        if ("...".equals(symbolName)) {
            return null;
        }
        if ("_".equals(symbolName)) {
            return Map.of();
        }
        if (literalIdentifiers.contains(symbolName)) {
            if (input.expression() instanceof SymbolExpression symbol && symbolName.equals(symbol.name())) {
                return Map.of();
            }
            return null;
        }
        return Map.of(symbolName, new SinglePatternBinding(input));
    }

    private static Map<String, PatternBinding> aggregateRepeatedBindings(
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

    private static Set<String> collectPatternVariables(
            SchemeExpression pattern,
            Set<String> literalIdentifiers
    ) {
        Set<String> variables = new HashSet<>();
        collectPatternVariables(pattern, literalIdentifiers, variables);
        return variables;
    }

    private static void collectPatternVariables(
            SchemeExpression pattern,
            Set<String> literalIdentifiers,
            Set<String> variables
    ) {
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
            collectPatternVariables(element, literalIdentifiers, variables);
        }
    }

    private static List<SchemeExpression> expandTemplateSequence(
            List<SchemeExpression> templates,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath,
            SyntaxContext defaultContext
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
                            appendIndex(repetitionPath, repeatIndex),
                            defaultContext
                    ));
                }
                index++;
                continue;
            }
            expanded.add(expandTemplate(template, bindings, repetitionPath, defaultContext));
        }
        return expanded;
    }

    private static SchemeExpression expandSymbol(
            SymbolExpression symbol,
            Map<String, PatternBinding> bindings,
            List<Integer> repetitionPath,
            SyntaxContext defaultContext
    ) throws EvalError {
        PatternBinding binding = bindings.get(symbol.name());
        if (binding != null) {
            return resolveBinding(binding, repetitionPath, symbol.name());
        }

        String symbolName = symbol.name();
        if ("...".equals(symbolName) || CORE_SYNTAX.contains(symbolName)) {
            return symbol;
        }
        if (defaultContext instanceof UseSiteSyntaxContext) {
            return symbol;
        }
        return ((SyntaxTemplateContext) defaultContext).aliasSymbol(symbol);
    }

    private static int determineRepeatCount(
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
                throw new EvalError("syntax-case: mismatched ellipsis lengths");
            }
        }

        if (repeatCount < 0) {
            throw new EvalError("syntax-case: ellipsis template has no repeated pattern variables");
        }
        return repeatCount;
    }

    private static void collectTemplateVariables(
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

    private static int repeatCount(PatternBinding binding, List<Integer> repetitionPath) throws EvalError {
        PatternBinding current = descend(binding, repetitionPath);
        if (current instanceof RepeatedPatternBinding repeated) {
            return repeated.items().size();
        }
        return -1;
    }

    private static SchemeExpression resolveBinding(
            PatternBinding binding,
            List<Integer> repetitionPath,
            String name
    ) throws EvalError {
        PatternBinding current = descend(binding, repetitionPath);
        if (current instanceof SinglePatternBinding single) {
            return single.syntax().expression();
        }
        throw new EvalError("syntax-case: pattern variable " + name + " used outside ellipsis");
    }

    private static PatternBinding descend(PatternBinding binding, List<Integer> repetitionPath) throws EvalError {
        PatternBinding current = binding;
        for (Integer index : repetitionPath) {
            if (!(current instanceof RepeatedPatternBinding repeated)) {
                throw new EvalError("syntax-case: invalid ellipsis nesting");
            }
            if (index < 0 || index >= repeated.items().size()) {
                throw new EvalError("syntax-case: ellipsis index out of bounds");
            }
            current = repeated.items().get(index);
        }
        return current;
    }

    private static List<Integer> appendIndex(List<Integer> repetitionPath, int index) {
        List<Integer> extendedPath = new ArrayList<>(repetitionPath.size() + 1);
        extendedPath.addAll(repetitionPath);
        extendedPath.add(index);
        return List.copyOf(extendedPath);
    }

    private static boolean bindingsEqual(PatternBinding left, PatternBinding right) {
        if (left instanceof SinglePatternBinding leftSingle && right instanceof SinglePatternBinding rightSingle) {
            return expressionsEqual(leftSingle.syntax().expression(), rightSingle.syntax().expression());
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

    private static boolean expressionsEqual(SchemeExpression left, SchemeExpression right) {
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

    private static boolean literalValuesEqual(SchemeValue left, SchemeValue right) {
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

    private static boolean isQuotedForm(SchemeExpression expression) {
        if (!(expression instanceof ListExpression list) || list.elements().size() != 2) {
            return false;
        }
        return list.elements().getFirst() instanceof SymbolExpression symbol
                && "quote".equals(symbol.name());
    }

    private static boolean isEllipsis(SchemeExpression expression) {
        return expression instanceof SymbolExpression symbol && "...".equals(symbol.name());
    }

    private static List<SyntaxValue> syntaxElements(List<SchemeExpression> expressions, SyntaxContext context) {
        List<SyntaxValue> values = new ArrayList<>(expressions.size());
        for (SchemeExpression expression : expressions) {
            values.add(new SyntaxValue(expression, context));
        }
        return List.copyOf(values);
    }

    sealed interface PatternBinding permits SinglePatternBinding, RepeatedPatternBinding {
    }

    record SinglePatternBinding(SyntaxValue syntax) implements PatternBinding {
    }

    record RepeatedPatternBinding(List<PatternBinding> items) implements PatternBinding {
    }
}
