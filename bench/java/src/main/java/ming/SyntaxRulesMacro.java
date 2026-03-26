package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@FunctionalInterface
interface SyntheticNameGenerator {
    String freshName(String kind, String base);
}

interface MacroBinding {
    Expr expand(ListExpr invocation) throws EvalError;
}

final class SyntaxRulesMacro implements MacroBinding {
    private final String name;
    private final Environment definitionEnv;
    private final Set<String> literals;
    private final List<SyntaxRule> rules;
    private final SyntheticNameGenerator nameGenerator;

    private SyntaxRulesMacro(String name, Environment definitionEnv, Set<String> literals,
                             List<SyntaxRule> rules, SyntheticNameGenerator nameGenerator) {
        this.name = name;
        this.definitionEnv = definitionEnv;
        this.literals = Set.copyOf(literals);
        this.rules = List.copyOf(rules);
        this.nameGenerator = nameGenerator;
    }

    static SyntaxRulesMacro compile(String macroName, Expr transformerExpr, Environment env,
                                    SyntheticNameGenerator nameGenerator) throws EvalError {
        if (!(transformerExpr instanceof ListExpr syntaxRulesExpr)) {
            throw new EvalError("define-syntax transformer must be a syntax-rules form");
        }

        List<Expr> forms = syntaxRulesExpr.elements();
        if (forms.isEmpty()
                || !(forms.getFirst() instanceof SymbolExpr head)
                || !head.name().equals("syntax-rules")) {
            throw new EvalError("define-syntax transformer must be a syntax-rules form");
        }
        if (forms.size() < 3) {
            throw new EvalError("syntax-rules requires literals and at least one rule");
        }
        if (!(forms.get(1) instanceof ListExpr literalExprs)) {
            throw new EvalError("syntax-rules literals must be a list");
        }

        Set<String> literals = parseLiterals(literalExprs);
        List<SyntaxRule> rules = parseRules(forms.subList(2, forms.size()));
        return new SyntaxRulesMacro(macroName, env, literals, rules, nameGenerator);
    }

    private static Set<String> parseLiterals(ListExpr literalExprs) throws EvalError {
        Set<String> literals = new HashSet<>();
        for (Expr literalExpr : literalExprs.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw new EvalError("syntax-rules literal must be a symbol");
            }
            literals.add(literalSymbol.name());
        }
        return literals;
    }

    private static List<SyntaxRule> parseRules(List<Expr> ruleExprs) throws EvalError {
        List<SyntaxRule> rules = new ArrayList<>(ruleExprs.size());
        for (Expr ruleExpr : ruleExprs) {
            if (!(ruleExpr instanceof ListExpr ruleList)) {
                throw new EvalError("syntax-rules rule must be a list");
            }

            List<Expr> parts = ruleList.elements();
            if (parts.size() != 2) {
                throw new EvalError("syntax-rules rule must contain a pattern and template");
            }
            rules.add(new SyntaxRule(parts.get(0), parts.get(1)));
        }
        return rules;
    }

    @Override
    public Expr expand(ListExpr invocation) throws EvalError {
        for (SyntaxRule rule : rules) {
            PatternMatch match = matchMacroRule(rule, invocation);
            if (match == null) {
                continue;
            }
            ExpansionContext context = new ExpansionContext(definitionEnv);
            return expandTemplate(rule.template(), match, context, null, false);
        }
        throw new EvalError("no matching rule for macro: " + name);
    }

    private PatternMatch matchMacroRule(SyntaxRule rule, ListExpr invocation) throws EvalError {
        if (!(rule.pattern() instanceof ListExpr patternList)) {
            throw new EvalError("syntax-rules pattern must be a list");
        }
        if (patternList.elements().isEmpty() || invocation.elements().isEmpty()) {
            return null;
        }

        PatternMatch match = new PatternMatch();
        if (!matchPatternElements(patternList.elements(), 1, invocation.elements(), 1, match)) {
            return null;
        }
        return match;
    }

    private boolean matchPattern(Expr pattern, Expr input, PatternMatch match) throws EvalError {
        return switch (pattern) {
            case IntExpr intExpr ->
                    input instanceof IntExpr inputInt && inputInt.value() == intExpr.value();
            case RationalExpr rationalExpr ->
                    input instanceof RationalExpr inputRational
                            && rationalExpr.numerator().equals(inputRational.numerator())
                            && rationalExpr.denominator().equals(inputRational.denominator());
            case InexactExpr inexactExpr ->
                    input instanceof InexactExpr inputInexact
                            && Double.compare(inexactExpr.value(), inputInexact.value()) == 0;
            case BoolExpr boolExpr ->
                    input instanceof BoolExpr inputBool && inputBool.value() == boolExpr.value();
            case StringExpr stringExpr ->
                    input instanceof StringExpr inputString
                            && inputString.value().equals(stringExpr.value());
            case CharExpr charExpr ->
                    input instanceof CharExpr inputChar && inputChar.value() == charExpr.value();
            case SymbolExpr symbolExpr -> matchPatternSymbol(symbolExpr.name(), input, match);
            case ListExpr listExpr -> input instanceof ListExpr inputList
                    && matchPatternElements(listExpr.elements(), 0, inputList.elements(), 0,
                    match);
        };
    }

    private boolean matchPatternSymbol(String symbol, Expr input, PatternMatch match)
            throws EvalError {
        if (symbol.equals("_")) {
            return true;
        }
        if (literals.contains(symbol)) {
            return input instanceof SymbolExpr inputSymbol && inputSymbol.name().equals(symbol);
        }
        return match.bindSingle(symbol, input);
    }

    private boolean matchPatternElements(List<Expr> patternElements, int patternIndex,
                                         List<Expr> inputElements, int inputIndex,
                                         PatternMatch match) throws EvalError {
        if (patternIndex >= patternElements.size()) {
            return inputIndex == inputElements.size();
        }

        Expr pattern = patternElements.get(patternIndex);
        if (patternIndex + 1 < patternElements.size()
                && isEllipsis(patternElements.get(patternIndex + 1))) {
            Set<String> repeatedNames = new HashSet<>();
            collectPatternVariables(pattern, repeatedNames);
            int maxRepeat = inputElements.size() - inputIndex
                    - minimumPatternArity(patternElements, patternIndex + 2);
            if (maxRepeat < 0) {
                return false;
            }

            for (int repeatCount = 0; repeatCount <= maxRepeat; repeatCount++) {
                PatternMatch candidate = match.copy();
                candidate.ensureRepeatedBindings(repeatedNames);
                boolean matched = true;

                for (int repeatIndex = 0; repeatIndex < repeatCount; repeatIndex++) {
                    PatternMatch repetition = new PatternMatch();
                    if (!matchPattern(pattern, inputElements.get(inputIndex + repeatIndex),
                            repetition)) {
                        matched = false;
                        break;
                    }
                    candidate.addRepetition(repetition);
                }

                if (matched && matchPatternElements(patternElements, patternIndex + 2,
                        inputElements, inputIndex + repeatCount, candidate)) {
                    match.replaceWith(candidate);
                    return true;
                }
            }
            return false;
        }

        if (inputIndex >= inputElements.size()) {
            return false;
        }
        if (!matchPattern(pattern, inputElements.get(inputIndex), match)) {
            return false;
        }
        return matchPatternElements(patternElements, patternIndex + 1, inputElements,
                inputIndex + 1, match);
    }

    private int minimumPatternArity(List<Expr> patternElements, int startIndex) {
        int required = 0;
        for (int index = startIndex; index < patternElements.size(); index++) {
            if (index + 1 < patternElements.size() && isEllipsis(patternElements.get(index + 1))) {
                index++;
                continue;
            }
            required++;
        }
        return required;
    }

    private void collectPatternVariables(Expr pattern, Set<String> variables) {
        switch (pattern) {
            case SymbolExpr symbolExpr -> {
                String name = symbolExpr.name();
                if (!name.equals("_") && !name.equals("...") && !literals.contains(name)) {
                    variables.add(name);
                }
            }
            case ListExpr listExpr -> {
                for (Expr element : listExpr.elements()) {
                    collectPatternVariables(element, variables);
                }
            }
            case IntExpr intExpr -> {
            }
            case RationalExpr rationalExpr -> {
            }
            case InexactExpr inexactExpr -> {
            }
            case BoolExpr boolExpr -> {
            }
            case StringExpr stringExpr -> {
            }
            case CharExpr charExpr -> {
            }
        }
    }

    private Expr expandTemplate(Expr template, PatternMatch match, ExpansionContext context,
                                Integer repeatIndex, boolean datumContext) throws EvalError {
        return switch (template) {
            case IntExpr intExpr -> intExpr;
            case RationalExpr rationalExpr -> rationalExpr;
            case InexactExpr inexactExpr -> inexactExpr;
            case BoolExpr boolExpr -> boolExpr;
            case StringExpr stringExpr -> stringExpr;
            case CharExpr charExpr -> charExpr;
            case SymbolExpr symbolExpr ->
                    expandTemplateSymbol(symbolExpr, match, context, repeatIndex, datumContext);
            case ListExpr listExpr ->
                    expandTemplateList(listExpr, match, context, repeatIndex, datumContext);
        };
    }

    private Expr expandTemplateSymbol(SymbolExpr symbolExpr, PatternMatch match,
                                      ExpansionContext context, Integer repeatIndex,
                                      boolean datumContext) throws EvalError {
        if (match.hasSingle(symbolExpr.name())) {
            return match.single(symbolExpr.name());
        }
        if (match.hasRepeated(symbolExpr.name())) {
            if (repeatIndex == null) {
                throw new EvalError(
                        "template uses repeated pattern variable without ellipsis: "
                                + symbolExpr.name());
            }
            return match.repeated(symbolExpr.name()).get(repeatIndex);
        }
        if (datumContext) {
            return symbolExpr;
        }
        return context.expandFreeIdentifier(symbolExpr);
    }

    private Expr expandTemplateList(ListExpr listExpr, PatternMatch match, ExpansionContext context,
                                    Integer repeatIndex, boolean datumContext) throws EvalError {
        if (!datumContext && !listExpr.elements().isEmpty()
                && listExpr.elements().getFirst() instanceof SymbolExpr headSymbol) {
            if (headSymbol.name().equals("quote") && listExpr.elements().size() == 2) {
                List<Expr> expanded = new ArrayList<>(2);
                expanded.add(headSymbol);
                expanded.add(expandTemplate(listExpr.elements().get(1), match, context,
                        repeatIndex, true));
                return new ListExpr(expanded, listExpr.position());
            }
            if (headSymbol.name().equals("lambda")) {
                return expandLambdaTemplate(listExpr, match, context, repeatIndex);
            }
            if (headSymbol.name().equals("let")) {
                return expandLetTemplate(listExpr, match, context, repeatIndex);
            }
        }

        List<Expr> expanded = new ArrayList<>();
        List<Expr> elements = listExpr.elements();
        for (int index = 0; index < elements.size(); index++) {
            Expr element = elements.get(index);
            if (index + 1 < elements.size() && isEllipsis(elements.get(index + 1))) {
                int count = repetitionCount(element, match);
                for (int repetition = 0; repetition < count; repetition++) {
                    expanded.add(expandTemplate(element, match, context, repetition, datumContext));
                }
                index++;
                continue;
            }
            expanded.add(expandTemplate(element, match, context, repeatIndex, datumContext));
        }
        return new ListExpr(expanded, listExpr.position());
    }

    private Expr expandLambdaTemplate(ListExpr listExpr, PatternMatch match,
                                      ExpansionContext context, Integer repeatIndex)
            throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.size() < 3) {
            throw new EvalError("lambda template requires parameters and a body");
        }

        BindingScope params = expandLambdaParameters(elements.get(1), match, context, repeatIndex);
        ExpansionContext bodyContext = context.child(params.renames());

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());
        expanded.add(params.expr());
        for (int index = 2; index < elements.size(); index++) {
            expanded.add(expandTemplate(elements.get(index), match, bodyContext, repeatIndex,
                    false));
        }
        return new ListExpr(expanded, listExpr.position());
    }

    private BindingScope expandLambdaParameters(Expr paramsExpr, PatternMatch match,
                                                ExpansionContext context, Integer repeatIndex)
            throws EvalError {
        if (paramsExpr instanceof SymbolExpr symbolExpr) {
            return expandBindingIdentifier(symbolExpr, match, context, repeatIndex);
        }
        if (!(paramsExpr instanceof ListExpr paramsList)) {
            throw new EvalError("lambda parameters must expand to a symbol or list");
        }

        List<Expr> expanded = new ArrayList<>(paramsList.elements().size());
        Map<String, String> renames = new HashMap<>();
        for (Expr paramExpr : paramsList.elements()) {
            if (paramExpr instanceof SymbolExpr dotSymbol && dotSymbol.name().equals(".")) {
                expanded.add(dotSymbol);
                continue;
            }
            BindingScope param = expandBindingIdentifier(paramExpr, match, context, repeatIndex);
            expanded.add(param.expr());
            renames.putAll(param.renames());
        }
        return new BindingScope(new ListExpr(expanded, paramsList.position()), renames);
    }

    private Expr expandLetTemplate(ListExpr listExpr, PatternMatch match, ExpansionContext context,
                                   Integer repeatIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.size() < 3) {
            throw new EvalError("let template requires bindings and a body");
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());

        int bodyIndex;
        ExpansionContext bodyContext = context;

        if (elements.get(1) instanceof SymbolExpr nameExpr) {
            BindingScope namedLet = expandBindingIdentifier(nameExpr, match, context, repeatIndex);
            expanded.add(namedLet.expr());
            bodyContext = context.child(namedLet.renames());

            BindingScope bindings = expandLetBindings(elements.get(2), match, context,
                    repeatIndex);
            expanded.add(bindings.expr());
            bodyContext = bodyContext.child(bindings.renames());
            bodyIndex = 3;
        } else {
            BindingScope bindings = expandLetBindings(elements.get(1), match, context,
                    repeatIndex);
            expanded.add(bindings.expr());
            bodyContext = context.child(bindings.renames());
            bodyIndex = 2;
        }

        for (int index = bodyIndex; index < elements.size(); index++) {
            expanded.add(expandTemplate(elements.get(index), match, bodyContext, repeatIndex,
                    false));
        }
        return new ListExpr(expanded, listExpr.position());
    }

    private BindingScope expandLetBindings(Expr bindingsExpr, PatternMatch match,
                                           ExpansionContext context, Integer repeatIndex)
            throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingList)) {
            throw new EvalError("let bindings must expand to a list");
        }

        List<Expr> expanded = new ArrayList<>();
        Map<String, String> renames = new HashMap<>();
        List<Expr> bindingExprs = bindingList.elements();
        for (int index = 0; index < bindingExprs.size(); index++) {
            Expr bindingExpr = bindingExprs.get(index);
            if (index + 1 < bindingExprs.size() && isEllipsis(bindingExprs.get(index + 1))) {
                int count = repetitionCount(bindingExpr, match);
                for (int repetition = 0; repetition < count; repetition++) {
                    BindingScope binding = expandSingleLetBinding(bindingExpr, match, context,
                            repetition);
                    expanded.add(binding.expr());
                    renames.putAll(binding.renames());
                }
                index++;
                continue;
            }

            BindingScope binding = expandSingleLetBinding(bindingExpr, match, context, repeatIndex);
            expanded.add(binding.expr());
            renames.putAll(binding.renames());
        }
        return new BindingScope(new ListExpr(expanded, bindingList.position()), renames);
    }

    private BindingScope expandSingleLetBinding(Expr bindingExpr, PatternMatch match,
                                                ExpansionContext context, Integer repeatIndex)
            throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList) || bindingList.elements().size() != 2) {
            throw new EvalError("let binding template must contain a name and value");
        }

        BindingScope bindingName = expandBindingIdentifier(bindingList.elements().getFirst(), match,
                context, repeatIndex);
        Expr bindingValue = expandTemplate(bindingList.elements().get(1), match, context,
                repeatIndex, false);

        return new BindingScope(new ListExpr(List.of(bindingName.expr(), bindingValue),
                bindingList.position()), bindingName.renames());
    }

    private BindingScope expandBindingIdentifier(Expr identifierExpr, PatternMatch match,
                                                 ExpansionContext context, Integer repeatIndex)
            throws EvalError {
        if (!(identifierExpr instanceof SymbolExpr identifierSymbol)) {
            throw new EvalError("macro binding must expand to a symbol");
        }

        if (match.hasSingle(identifierSymbol.name())) {
            Expr bound = match.single(identifierSymbol.name());
            if (!(bound instanceof SymbolExpr)) {
                throw new EvalError("macro binding must expand to a symbol");
            }
            return new BindingScope(bound, Map.of());
        }
        if (match.hasRepeated(identifierSymbol.name())) {
            if (repeatIndex == null) {
                throw new EvalError(
                        "macro binding uses repeated pattern variable without ellipsis: "
                                + identifierSymbol.name());
            }
            Expr bound = match.repeated(identifierSymbol.name()).get(repeatIndex);
            if (!(bound instanceof SymbolExpr)) {
                throw new EvalError("macro binding must expand to a symbol");
            }
            return new BindingScope(bound, Map.of());
        }

        String freshName = freshSyntheticName("bind", identifierSymbol.name());
        return new BindingScope(new SymbolExpr(freshName, identifierSymbol.position()),
                Map.of(identifierSymbol.name(), freshName));
    }

    private int repetitionCount(Expr template, PatternMatch match) throws EvalError {
        Set<String> repeatedNames = new HashSet<>();
        collectRepeatedTemplateVariables(template, match, repeatedNames);
        if (repeatedNames.isEmpty()) {
            throw new EvalError("ellipsis template has no repeated pattern variables");
        }

        Integer count = null;
        for (String name : repeatedNames) {
            int current = match.repeated(name).size();
            if (count == null) {
                count = current;
                continue;
            }
            if (count != current) {
                throw new EvalError("mismatched ellipsis lengths in macro template");
            }
        }
        return count == null ? 0 : count;
    }

    private void collectRepeatedTemplateVariables(Expr template, PatternMatch match,
                                                  Set<String> repeatedNames) {
        switch (template) {
            case SymbolExpr symbolExpr -> {
                if (match.hasRepeated(symbolExpr.name())) {
                    repeatedNames.add(symbolExpr.name());
                }
            }
            case ListExpr listExpr -> {
                for (Expr element : listExpr.elements()) {
                    collectRepeatedTemplateVariables(element, match, repeatedNames);
                }
            }
            case IntExpr intExpr -> {
            }
            case RationalExpr rationalExpr -> {
            }
            case InexactExpr inexactExpr -> {
            }
            case BoolExpr boolExpr -> {
            }
            case StringExpr stringExpr -> {
            }
            case CharExpr charExpr -> {
            }
        }
    }

    private String freshSyntheticName(String kind, String base) {
        return nameGenerator.freshName(kind, base);
    }

    private static boolean isEllipsis(Expr expr) {
        return expr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("...");
    }

    private static boolean sameSyntax(Expr left, Expr right) {
        if (left == right) {
            return true;
        }
        return switch (left) {
            case IntExpr intExpr -> right instanceof IntExpr rightInt
                    && intExpr.value() == rightInt.value();
            case RationalExpr rationalExpr -> right instanceof RationalExpr rightRational
                    && rationalExpr.numerator().equals(rightRational.numerator())
                    && rationalExpr.denominator().equals(rightRational.denominator());
            case InexactExpr inexactExpr -> right instanceof InexactExpr rightInexact
                    && Double.compare(inexactExpr.value(), rightInexact.value()) == 0;
            case BoolExpr boolExpr -> right instanceof BoolExpr rightBool
                    && boolExpr.value() == rightBool.value();
            case StringExpr stringExpr -> right instanceof StringExpr rightString
                    && stringExpr.value().equals(rightString.value());
            case CharExpr charExpr -> right instanceof CharExpr rightChar
                    && charExpr.value() == rightChar.value();
            case SymbolExpr symbolExpr -> right instanceof SymbolExpr rightSymbol
                    && symbolExpr.name().equals(rightSymbol.name());
            case ListExpr listExpr -> {
                if (!(right instanceof ListExpr rightList)
                        || listExpr.elements().size() != rightList.elements().size()) {
                    yield false;
                }
                boolean equal = true;
                for (int index = 0; index < listExpr.elements().size(); index++) {
                    if (!sameSyntax(listExpr.elements().get(index), rightList.elements().get(index))) {
                        equal = false;
                        break;
                    }
                }
                yield equal;
            }
        };
    }

    private static boolean isCoreSyntax(String name) {
        return switch (name) {
            case "define", "define-syntax", "set!", "if", "quote", "lambda", "begin", "let",
                    "cond", "and", "or", "syntax-rules" -> true;
            default -> false;
        };
    }

    private record SyntaxRule(Expr pattern, Expr template) {
    }

    private record BindingScope(Expr expr, Map<String, String> renames) {
        BindingScope {
            renames = Map.copyOf(renames);
        }
    }

    private static final class PatternMatch {
        private final Map<String, Expr> singleBindings = new HashMap<>();
        private final Map<String, List<Expr>> repeatedBindings = new HashMap<>();

        private PatternMatch copy() {
            PatternMatch copy = new PatternMatch();
            copy.replaceWith(this);
            return copy;
        }

        private void replaceWith(PatternMatch other) {
            singleBindings.clear();
            singleBindings.putAll(other.singleBindings);

            repeatedBindings.clear();
            for (Map.Entry<String, List<Expr>> entry : other.repeatedBindings.entrySet()) {
                repeatedBindings.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
        }

        private boolean bindSingle(String name, Expr expr) {
            Expr existing = singleBindings.get(name);
            if (existing == null) {
                singleBindings.put(name, expr);
                return true;
            }
            return sameSyntax(existing, expr);
        }

        private void addRepetition(PatternMatch repetition) throws EvalError {
            if (!repetition.repeatedBindings.isEmpty()) {
                throw new EvalError("nested ellipsis patterns are not supported");
            }
            for (Map.Entry<String, Expr> entry : repetition.singleBindings.entrySet()) {
                repeatedBindings.computeIfAbsent(entry.getKey(), ignored -> new ArrayList<>())
                        .add(entry.getValue());
            }
        }

        private void ensureRepeatedBindings(Set<String> names) {
            for (String name : names) {
                repeatedBindings.computeIfAbsent(name, ignored -> new ArrayList<>());
            }
        }

        private boolean hasSingle(String name) {
            return singleBindings.containsKey(name);
        }

        private Expr single(String name) {
            return singleBindings.get(name);
        }

        private boolean hasRepeated(String name) {
            return repeatedBindings.containsKey(name);
        }

        private List<Expr> repeated(String name) {
            return repeatedBindings.get(name);
        }
    }

    private final class ExpansionContext {
        private final Environment definitionEnv;
        private final Map<String, String> lexicalRenames;
        private final Map<String, String> capturedValueAliases;
        private final Map<String, String> capturedSyntaxAliases;
        private final Map<String, String> introducedIdentifiers;

        private ExpansionContext(Environment definitionEnv) {
            this(definitionEnv, new HashMap<>(), new HashMap<>(), new HashMap<>(),
                    new HashMap<>());
        }

        private ExpansionContext(Environment definitionEnv, Map<String, String> lexicalRenames,
                                 Map<String, String> capturedValueAliases,
                                 Map<String, String> capturedSyntaxAliases,
                                 Map<String, String> introducedIdentifiers) {
            this.definitionEnv = definitionEnv;
            this.lexicalRenames = lexicalRenames;
            this.capturedValueAliases = capturedValueAliases;
            this.capturedSyntaxAliases = capturedSyntaxAliases;
            this.introducedIdentifiers = introducedIdentifiers;
        }

        private ExpansionContext child(Map<String, String> additionalRenames) {
            Map<String, String> nextRenames = new HashMap<>(lexicalRenames);
            nextRenames.putAll(additionalRenames);
            return new ExpansionContext(definitionEnv, nextRenames, capturedValueAliases,
                    capturedSyntaxAliases, introducedIdentifiers);
        }

        private SymbolExpr expandFreeIdentifier(SymbolExpr symbolExpr) {
            String lexicalName = lexicalRenames.get(symbolExpr.name());
            if (lexicalName != null) {
                return new SymbolExpr(lexicalName, symbolExpr.position());
            }
            if (isCoreSyntax(symbolExpr.name())) {
                return symbolExpr;
            }

            MacroBinding syntaxBinding = definitionEnv.lookupSyntax(symbolExpr.name());
            if (syntaxBinding != null) {
                String alias = capturedSyntaxAliases.computeIfAbsent(symbolExpr.name(), name -> {
                    String freshName = freshSyntheticName("syn", name);
                    definitionEnv.defineSyntaxAlias(freshName, syntaxBinding);
                    return freshName;
                });
                return new SymbolExpr(alias, symbolExpr.position());
            }

            Cell valueBinding = definitionEnv.lookupCellOrNull(symbolExpr.name());
            if (valueBinding != null) {
                String alias = capturedValueAliases.computeIfAbsent(symbolExpr.name(), name -> {
                    String freshName = freshSyntheticName("cap", name);
                    definitionEnv.defineAlias(freshName, valueBinding);
                    return freshName;
                });
                return new SymbolExpr(alias, symbolExpr.position());
            }

            String freshName = introducedIdentifiers.computeIfAbsent(symbolExpr.name(),
                    name -> freshSyntheticName("gen", name));
            return new SymbolExpr(freshName, symbolExpr.position());
        }
    }
}
