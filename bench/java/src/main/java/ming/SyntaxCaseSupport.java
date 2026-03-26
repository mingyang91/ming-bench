package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class TransformerProcedureMacro implements MacroBinding {
    private final Evaluator evaluator;
    private final ProcedureValue transformer;
    private final Environment definitionEnv;

    TransformerProcedureMacro(Evaluator evaluator, ProcedureValue transformer,
                              Environment definitionEnv) {
        this.evaluator = evaluator;
        this.transformer = transformer;
        this.definitionEnv = definitionEnv;
    }

    @Override
    public Expr expand(ListExpr invocation) throws EvalError {
        Value result = evaluator.applyTransformerProcedure(transformer,
                new SyntaxValue(invocation), definitionEnv);
        if (result instanceof SyntaxValue syntaxValue) {
            return syntaxValue.expr();
        }
        throw new EvalError("macro transformer must return syntax");
    }
}

final class SyntaxCaseSupport {
    private SyntaxCaseSupport() {
    }

    static Set<String> parseLiteralIdentifiers(Expr literalExpr) throws EvalError {
        if (!(literalExpr instanceof ListExpr literalList)) {
            throw new EvalError("syntax-case literals must be a list");
        }

        Set<String> literals = new HashSet<>();
        for (Expr literal : literalList.elements()) {
            if (!(literal instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("syntax-case literal must be a symbol");
            }
            literals.add(symbolExpr.name());
        }
        return Set.copyOf(literals);
    }

    static SyntaxMatch match(Expr pattern, Expr input, Set<String> literals) throws EvalError {
        SyntaxMatch match = new SyntaxMatch();
        if (!matchPattern(pattern, input, literals, match)) {
            return null;
        }
        return match;
    }

    static void bindPatternVariables(Environment env, SyntaxMatch match) {
        for (Map.Entry<String, Expr> entry : match.singleBindings().entrySet()) {
            env.define(entry.getKey(), new SyntaxValue(entry.getValue()));
        }
        for (Map.Entry<String, List<Expr>> entry : match.repeatedBindings().entrySet()) {
            env.define(entry.getKey(), new SyntaxSequenceValue(entry.getValue()));
        }
    }

    static Expr expandTemplate(Expr template, Environment patternEnv, Environment definitionEnv,
                               SyntheticNameGenerator nameGenerator) throws EvalError {
        return new TemplateExpander(patternEnv, definitionEnv, nameGenerator).expand(template);
    }

    private static boolean matchPattern(Expr pattern, Expr input, Set<String> literals,
                                        SyntaxMatch match) throws EvalError {
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
            case SymbolExpr symbolExpr -> matchPatternSymbol(symbolExpr.name(), input, literals,
                    match);
            case ListExpr listExpr -> input instanceof ListExpr inputList
                    && matchPatternElements(listExpr.elements(), 0, inputList.elements(), 0,
                    literals, match);
        };
    }

    private static boolean matchPatternSymbol(String symbol, Expr input, Set<String> literals,
                                              SyntaxMatch match) throws EvalError {
        if (symbol.equals("_")) {
            return true;
        }
        if (literals.contains(symbol)) {
            return input instanceof SymbolExpr inputSymbol && inputSymbol.name().equals(symbol);
        }
        return match.bindSingle(symbol, input);
    }

    private static boolean matchPatternElements(List<Expr> patternElements, int patternIndex,
                                                List<Expr> inputElements, int inputIndex,
                                                Set<String> literals, SyntaxMatch match)
            throws EvalError {
        if (patternIndex >= patternElements.size()) {
            return inputIndex == inputElements.size();
        }

        Expr pattern = patternElements.get(patternIndex);
        if (patternIndex + 1 < patternElements.size()
                && isEllipsis(patternElements.get(patternIndex + 1))) {
            Set<String> repeatedNames = new HashSet<>();
            collectPatternVariables(pattern, literals, repeatedNames);
            int maxRepeat = inputElements.size() - inputIndex
                    - minimumPatternArity(patternElements, patternIndex + 2);
            if (maxRepeat < 0) {
                return false;
            }

            for (int repeatCount = 0; repeatCount <= maxRepeat; repeatCount++) {
                SyntaxMatch candidate = match.copy();
                candidate.ensureRepeatedBindings(repeatedNames);
                boolean matched = true;

                for (int repeatIndex = 0; repeatIndex < repeatCount; repeatIndex++) {
                    SyntaxMatch repetition = new SyntaxMatch();
                    if (!matchPattern(pattern, inputElements.get(inputIndex + repeatIndex),
                            literals, repetition)) {
                        matched = false;
                        break;
                    }
                    candidate.addRepetition(repetition);
                }

                if (matched && matchPatternElements(patternElements, patternIndex + 2,
                        inputElements, inputIndex + repeatCount, literals, candidate)) {
                    match.replaceWith(candidate);
                    return true;
                }
            }
            return false;
        }

        if (inputIndex >= inputElements.size()) {
            return false;
        }
        if (!matchPattern(pattern, inputElements.get(inputIndex), literals, match)) {
            return false;
        }
        return matchPatternElements(patternElements, patternIndex + 1, inputElements,
                inputIndex + 1, literals, match);
    }

    private static int minimumPatternArity(List<Expr> patternElements, int startIndex) {
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

    private static void collectPatternVariables(Expr pattern, Set<String> literals,
                                                Set<String> variables) {
        switch (pattern) {
            case SymbolExpr symbolExpr -> {
                String name = symbolExpr.name();
                if (!name.equals("_") && !name.equals("...") && !literals.contains(name)) {
                    variables.add(name);
                }
            }
            case ListExpr listExpr -> {
                for (Expr element : listExpr.elements()) {
                    collectPatternVariables(element, literals, variables);
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
                    if (!sameSyntax(listExpr.elements().get(index),
                            rightList.elements().get(index))) {
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
            case "define", "define-syntax", "define-record-type", "set!", "if", "quote",
                    "lambda", "case-lambda", "begin", "let", "let*", "letrec", "letrec*",
                    "cond", "case", "and", "or", "do", "guard", "syntax-rules",
                    "syntax-case", "syntax", "with-syntax" -> true;
            default -> false;
        };
    }

    static final class SyntaxMatch {
        private final Map<String, Expr> singleBindings = new HashMap<>();
        private final Map<String, List<Expr>> repeatedBindings = new HashMap<>();

        private SyntaxMatch copy() {
            SyntaxMatch copy = new SyntaxMatch();
            copy.replaceWith(this);
            return copy;
        }

        private void replaceWith(SyntaxMatch other) {
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

        private void addRepetition(SyntaxMatch repetition) throws EvalError {
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

        Map<String, Expr> singleBindings() {
            return Map.copyOf(singleBindings);
        }

        Map<String, List<Expr>> repeatedBindings() {
            Map<String, List<Expr>> copy = new HashMap<>();
            for (Map.Entry<String, List<Expr>> entry : repeatedBindings.entrySet()) {
                copy.put(entry.getKey(), List.copyOf(entry.getValue()));
            }
            return Map.copyOf(copy);
        }
    }

    private static final class TemplateExpander {
        private final Environment patternEnv;
        private final SyntheticNameGenerator nameGenerator;
        private final ExpansionContext rootContext;

        private TemplateExpander(Environment patternEnv, Environment definitionEnv,
                                 SyntheticNameGenerator nameGenerator) {
            this.patternEnv = patternEnv;
            this.nameGenerator = nameGenerator;
            this.rootContext = new ExpansionContext(definitionEnv);
        }

        private Expr expand(Expr template) throws EvalError {
            return expand(template, rootContext, null, false);
        }

        private Expr expand(Expr template, ExpansionContext context, Integer repeatIndex,
                            boolean datumContext) throws EvalError {
            return switch (template) {
                case IntExpr intExpr -> intExpr;
                case RationalExpr rationalExpr -> rationalExpr;
                case InexactExpr inexactExpr -> inexactExpr;
                case BoolExpr boolExpr -> boolExpr;
                case StringExpr stringExpr -> stringExpr;
                case CharExpr charExpr -> charExpr;
                case SymbolExpr symbolExpr ->
                        expandSymbol(symbolExpr, context, repeatIndex, datumContext);
                case ListExpr listExpr ->
                        expandList(listExpr, context, repeatIndex, datumContext);
            };
        }

        private Expr expandSymbol(SymbolExpr symbolExpr, ExpansionContext context,
                                  Integer repeatIndex, boolean datumContext) throws EvalError {
            Value patternBinding = patternBinding(symbolExpr.name());
            if (patternBinding instanceof SyntaxValue syntaxValue) {
                return syntaxValue.expr();
            }
            if (patternBinding instanceof SyntaxSequenceValue sequenceValue) {
                if (repeatIndex == null) {
                    throw new EvalError(
                            "template uses repeated pattern variable without ellipsis: "
                                    + symbolExpr.name());
                }
                return sequenceValue.expressions().get(repeatIndex);
            }
            if (datumContext) {
                return symbolExpr;
            }
            return context.expandFreeIdentifier(symbolExpr);
        }

        private Expr expandList(ListExpr listExpr, ExpansionContext context, Integer repeatIndex,
                                boolean datumContext) throws EvalError {
            List<Expr> elements = listExpr.elements();
            if (!datumContext && !elements.isEmpty()
                    && elements.getFirst() instanceof SymbolExpr headSymbol) {
                if (headSymbol.name().equals("quote") && elements.size() == 2) {
                    List<Expr> expanded = new ArrayList<>(2);
                    expanded.add(headSymbol);
                    expanded.add(expand(elements.get(1), context, repeatIndex, true));
                    return new ListExpr(expanded, listExpr.position());
                }
                if (headSymbol.name().equals("lambda")) {
                    return expandLambdaTemplate(listExpr, context, repeatIndex);
                }
                if (headSymbol.name().equals("let")) {
                    return expandLetTemplate(listExpr, context, repeatIndex);
                }
            }

            List<Expr> expanded = new ArrayList<>();
            for (int index = 0; index < elements.size(); index++) {
                Expr element = elements.get(index);
                if (index + 1 < elements.size() && isEllipsis(elements.get(index + 1))) {
                    int count = repetitionCount(element);
                    for (int repetition = 0; repetition < count; repetition++) {
                        expanded.add(expand(element, context, repetition, datumContext));
                    }
                    index++;
                    continue;
                }
                expanded.add(expand(element, context, repeatIndex, datumContext));
            }
            return new ListExpr(expanded, listExpr.position());
        }

        private Expr expandLambdaTemplate(ListExpr listExpr, ExpansionContext context,
                                          Integer repeatIndex) throws EvalError {
            List<Expr> elements = listExpr.elements();
            if (elements.size() < 3) {
                throw new EvalError("lambda template requires parameters and a body");
            }

            BindingScope params = expandLambdaParameters(elements.get(1), context, repeatIndex);
            ExpansionContext bodyContext = context.child(params.renames());

            List<Expr> expanded = new ArrayList<>(elements.size());
            expanded.add(elements.getFirst());
            expanded.add(params.expr());
            for (int index = 2; index < elements.size(); index++) {
                expanded.add(expand(elements.get(index), bodyContext, repeatIndex, false));
            }
            return new ListExpr(expanded, listExpr.position());
        }

        private BindingScope expandLambdaParameters(Expr paramsExpr, ExpansionContext context,
                                                    Integer repeatIndex) throws EvalError {
            if (paramsExpr instanceof SymbolExpr) {
                return expandBindingIdentifier(paramsExpr, context, repeatIndex);
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
                BindingScope param = expandBindingIdentifier(paramExpr, context, repeatIndex);
                expanded.add(param.expr());
                renames.putAll(param.renames());
            }
            return new BindingScope(new ListExpr(expanded, paramsList.position()), renames);
        }

        private Expr expandLetTemplate(ListExpr listExpr, ExpansionContext context,
                                       Integer repeatIndex) throws EvalError {
            List<Expr> elements = listExpr.elements();
            if (elements.size() < 3) {
                throw new EvalError("let template requires bindings and a body");
            }

            List<Expr> expanded = new ArrayList<>(elements.size());
            expanded.add(elements.getFirst());

            int bodyIndex;
            ExpansionContext bodyContext = context;

            if (elements.get(1) instanceof SymbolExpr) {
                BindingScope namedLet = expandBindingIdentifier(elements.get(1), context,
                        repeatIndex);
                expanded.add(namedLet.expr());
                bodyContext = context.child(namedLet.renames());

                BindingScope bindings = expandLetBindings(elements.get(2), context, repeatIndex);
                expanded.add(bindings.expr());
                bodyContext = bodyContext.child(bindings.renames());
                bodyIndex = 3;
            } else {
                BindingScope bindings = expandLetBindings(elements.get(1), context, repeatIndex);
                expanded.add(bindings.expr());
                bodyContext = context.child(bindings.renames());
                bodyIndex = 2;
            }

            for (int index = bodyIndex; index < elements.size(); index++) {
                expanded.add(expand(elements.get(index), bodyContext, repeatIndex, false));
            }
            return new ListExpr(expanded, listExpr.position());
        }

        private BindingScope expandLetBindings(Expr bindingsExpr, ExpansionContext context,
                                               Integer repeatIndex) throws EvalError {
            if (!(bindingsExpr instanceof ListExpr bindingList)) {
                throw new EvalError("let bindings must expand to a list");
            }

            List<Expr> expanded = new ArrayList<>();
            Map<String, String> renames = new HashMap<>();
            List<Expr> bindingExprs = bindingList.elements();
            for (int index = 0; index < bindingExprs.size(); index++) {
                Expr bindingExpr = bindingExprs.get(index);
                if (index + 1 < bindingExprs.size() && isEllipsis(bindingExprs.get(index + 1))) {
                    int count = repetitionCount(bindingExpr);
                    for (int repetition = 0; repetition < count; repetition++) {
                        BindingScope binding = expandSingleLetBinding(bindingExpr, context,
                                repetition);
                        expanded.add(binding.expr());
                        renames.putAll(binding.renames());
                    }
                    index++;
                    continue;
                }

                BindingScope binding = expandSingleLetBinding(bindingExpr, context, repeatIndex);
                expanded.add(binding.expr());
                renames.putAll(binding.renames());
            }
            return new BindingScope(new ListExpr(expanded, bindingList.position()), renames);
        }

        private BindingScope expandSingleLetBinding(Expr bindingExpr, ExpansionContext context,
                                                    Integer repeatIndex) throws EvalError {
            if (!(bindingExpr instanceof ListExpr bindingList) || bindingList.elements().size() != 2) {
                throw new EvalError("let binding template must contain a name and value");
            }

            BindingScope bindingName = expandBindingIdentifier(bindingList.elements().getFirst(),
                    context, repeatIndex);
            Expr bindingValue = expand(bindingList.elements().get(1), context, repeatIndex, false);

            return new BindingScope(new ListExpr(List.of(bindingName.expr(), bindingValue),
                    bindingList.position()), bindingName.renames());
        }

        private BindingScope expandBindingIdentifier(Expr identifierExpr, ExpansionContext context,
                                                     Integer repeatIndex) throws EvalError {
            if (!(identifierExpr instanceof SymbolExpr identifierSymbol)) {
                throw new EvalError("macro binding must expand to a symbol");
            }

            Value patternBinding = patternBinding(identifierSymbol.name());
            if (patternBinding instanceof SyntaxValue syntaxValue) {
                if (!(syntaxValue.expr() instanceof SymbolExpr)) {
                    throw new EvalError("macro binding must expand to a symbol");
                }
                return new BindingScope(syntaxValue.expr(), Map.of());
            }
            if (patternBinding instanceof SyntaxSequenceValue sequenceValue) {
                if (repeatIndex == null) {
                    throw new EvalError(
                            "macro binding uses repeated pattern variable without ellipsis: "
                                    + identifierSymbol.name());
                }
                Expr bound = sequenceValue.expressions().get(repeatIndex);
                if (!(bound instanceof SymbolExpr)) {
                    throw new EvalError("macro binding must expand to a symbol");
                }
                return new BindingScope(bound, Map.of());
            }

            String freshName = nameGenerator.freshName("bind", identifierSymbol.name());
            return new BindingScope(new SymbolExpr(freshName, identifierSymbol.position()),
                    Map.of(identifierSymbol.name(), freshName));
        }

        private int repetitionCount(Expr template) throws EvalError {
            Set<String> repeatedNames = new HashSet<>();
            collectRepeatedTemplateVariables(template, repeatedNames);
            if (repeatedNames.isEmpty()) {
                throw new EvalError("ellipsis template has no repeated pattern variables");
            }

            Integer count = null;
            for (String name : repeatedNames) {
                SyntaxSequenceValue sequenceValue = repeatedBinding(name);
                int current = sequenceValue.expressions().size();
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

        private void collectRepeatedTemplateVariables(Expr template, Set<String> repeatedNames) {
            switch (template) {
                case SymbolExpr symbolExpr -> {
                    if (patternBinding(symbolExpr.name()) instanceof SyntaxSequenceValue) {
                        repeatedNames.add(symbolExpr.name());
                    }
                }
                case ListExpr listExpr -> {
                    for (Expr element : listExpr.elements()) {
                        collectRepeatedTemplateVariables(element, repeatedNames);
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

        private Value patternBinding(String name) {
            Cell binding = patternEnv.lookupCellOrNull(name);
            return binding == null ? null : binding.value();
        }

        private SyntaxSequenceValue repeatedBinding(String name) {
            return (SyntaxSequenceValue) patternBinding(name);
        }

        private record BindingScope(Expr expr, Map<String, String> renames) {
            BindingScope {
                renames = Map.copyOf(renames);
            }
        }

        private final class ExpansionContext {
            private final Environment definitionEnv;
            private final Map<String, String> lexicalRenames;
            private final Map<String, String> capturedValueAliases;
            private final Map<String, String> capturedSyntaxAliases;

            private ExpansionContext(Environment definitionEnv) {
                this(definitionEnv, new HashMap<>(), new HashMap<>(), new HashMap<>());
            }

            private ExpansionContext(Environment definitionEnv, Map<String, String> lexicalRenames,
                                     Map<String, String> capturedValueAliases,
                                     Map<String, String> capturedSyntaxAliases) {
                this.definitionEnv = definitionEnv;
                this.lexicalRenames = lexicalRenames;
                this.capturedValueAliases = capturedValueAliases;
                this.capturedSyntaxAliases = capturedSyntaxAliases;
            }

            private ExpansionContext child(Map<String, String> additionalRenames) {
                Map<String, String> nextRenames = new HashMap<>(lexicalRenames);
                nextRenames.putAll(additionalRenames);
                return new ExpansionContext(definitionEnv, nextRenames, capturedValueAliases,
                        capturedSyntaxAliases);
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
                        String freshName = nameGenerator.freshName("syn", name);
                        definitionEnv.defineSyntaxAlias(freshName, syntaxBinding);
                        return freshName;
                    });
                    return new SymbolExpr(alias, symbolExpr.position());
                }

                Cell valueBinding = definitionEnv.lookupCellOrNull(symbolExpr.name());
                if (valueBinding != null) {
                    String alias = capturedValueAliases.computeIfAbsent(symbolExpr.name(), name -> {
                        String freshName = nameGenerator.freshName("cap", name);
                        definitionEnv.defineAlias(freshName, valueBinding);
                        return freshName;
                    });
                    return new SymbolExpr(alias, symbolExpr.position());
                }

                return symbolExpr;
            }
        }
    }
}
