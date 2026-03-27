package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

final class SyntaxRulesSupport {
    private static final AtomicLong MACRO_GENSYM_COUNTER = new AtomicLong();

    private SyntaxRulesSupport() {
    }

    static SyntaxRulesMacro parseSyntaxRules(String macroName, Expr transformerExpr, Environment env)
            throws EvalError {
        if (!(transformerExpr instanceof ListExpr transformerList)) {
            throw at(transformerExpr.loc(), "define-syntax expects a syntax-rules transformer");
        }
        if (transformerList.elements().isEmpty()) {
            throw at(transformerExpr.loc(), "syntax-rules form cannot be empty");
        }

        Expr headExpr = transformerList.elements().getFirst();
        if (!(headExpr instanceof SymbolExpr headSymbol)
                || !"syntax-rules".equals(headSymbol.name())) {
            throw at(headExpr.loc(), "define-syntax expects a syntax-rules transformer");
        }
        if (transformerList.elements().size() < 3) {
            throw at(transformerExpr.loc(), "syntax-rules requires literals and rules");
        }

        Expr literalsExpr = transformerList.elements().get(1);
        if (!(literalsExpr instanceof ListExpr literalsList)) {
            throw at(literalsExpr.loc(), "syntax-rules literals must be a list");
        }

        Set<String> literals = new HashSet<>(literalsList.elements().size());
        for (Expr literalExpr : literalsList.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw at(literalExpr.loc(), "syntax-rules literals must be symbols");
            }
            literals.add(literalSymbol.name());
        }

        List<MacroRule> rules = new ArrayList<>(transformerList.elements().size() - 2);
        for (int index = 2; index < transformerList.elements().size(); index++) {
            Expr ruleExpr = transformerList.elements().get(index);
            if (!(ruleExpr instanceof ListExpr ruleList)) {
                throw at(ruleExpr.loc(), "syntax-rules rules must be lists");
            }
            if (ruleList.elements().size() != 2) {
                throw at(ruleExpr.loc(),
                        "syntax-rules rules must contain a pattern and template");
            }

            Expr patternExpr = ruleList.elements().getFirst();
            if (!(patternExpr instanceof ListExpr patternList)) {
                throw at(patternExpr.loc(), "syntax-rules patterns must be lists");
            }
            if (patternList.elements().isEmpty()) {
                throw at(patternExpr.loc(), "syntax-rules pattern cannot be empty");
            }

            Expr keywordExpr = patternList.elements().getFirst();
            if (!(keywordExpr instanceof SymbolExpr keywordSymbol)) {
                throw at(keywordExpr.loc(), "syntax-rules pattern must start with a symbol");
            }
            if (!macroName.equals(keywordSymbol.name())) {
                throw at(keywordExpr.loc(), "syntax-rules pattern must start with " + macroName);
            }

            rules.add(new MacroRule(
                    List.copyOf(patternList.elements().subList(1, patternList.elements().size())),
                    ruleList.elements().get(1)
            ));
        }

        if (rules.isEmpty()) {
            throw at(transformerExpr.loc(), "syntax-rules requires at least one rule");
        }

        return new SyntaxRulesMacro(
                macroName,
                Set.copyOf(literals),
                List.copyOf(rules),
                env
        );
    }

    static MacroExpansion expandMacroCall(
            SyntaxRulesMacro definition,
            List<Expr> arguments,
            Environment callEnv,
            SourceLoc callLoc
    ) throws EvalError {
        for (MacroRule rule : definition.rules()) {
            Map<String, PatternBinding> bindings = new HashMap<>();
            if (matchListPattern(rule.pattern(), arguments, definition.literals(), bindings)) {
                Environment expansionEnv = new Environment(callEnv);
                TemplateExpander expander = new TemplateExpander(
                        definition,
                        expansionEnv,
                        bindings
                );
                return new MacroExpansion(expander.expand(rule.template()), expansionEnv);
            }
        }

        throw at(callLoc, "no matching syntax-rules clause for " + definition.name());
    }

    private static boolean matchListPattern(
            List<Expr> patternItems,
            List<Expr> inputItems,
            Set<String> literals,
            Map<String, PatternBinding> bindings
    ) {
        if (patternItems.size() >= 2 && isEllipsis(patternItems.get(patternItems.size() - 1))) {
            Expr repeatPattern = patternItems.get(patternItems.size() - 2);
            List<Expr> fixedPatterns = patternItems.subList(0, patternItems.size() - 2);
            if (inputItems.size() < fixedPatterns.size()) {
                return false;
            }

            for (int index = 0; index < fixedPatterns.size(); index++) {
                if (!matchPattern(fixedPatterns.get(index), inputItems.get(index), literals, bindings)) {
                    return false;
                }
            }

            return matchRepeatedPattern(
                    repeatPattern,
                    inputItems.subList(fixedPatterns.size(), inputItems.size()),
                    literals,
                    bindings
            );
        }

        if (patternItems.size() != inputItems.size()) {
            return false;
        }

        for (int index = 0; index < patternItems.size(); index++) {
            if (!matchPattern(patternItems.get(index), inputItems.get(index), literals, bindings)) {
                return false;
            }
        }
        return true;
    }

    private static boolean matchPattern(
            Expr pattern,
            Expr input,
            Set<String> literals,
            Map<String, PatternBinding> bindings
    ) {
        return switch (pattern) {
            case NumberExpr numberExpr -> input instanceof NumberExpr other
                    && numberExpr.value().equals(other.value());
            case BooleanExpr booleanExpr -> input instanceof BooleanExpr other
                    && booleanExpr.value() == other.value();
            case StringExpr stringExpr -> input instanceof StringExpr other
                    && stringExpr.value().equals(other.value());
            case CharExpr charExpr -> input instanceof CharExpr other
                    && charExpr.codePoint() == other.codePoint();
            case SymbolExpr symbolExpr -> matchSymbolPattern(symbolExpr.name(), input, literals, bindings);
            case ListExpr listExpr -> input instanceof ListExpr other
                    && matchListPattern(listExpr.elements(), other.elements(), literals, bindings);
        };
    }

    private static boolean matchSymbolPattern(
            String name,
            Expr input,
            Set<String> literals,
            Map<String, PatternBinding> bindings
    ) {
        if ("...".equals(name)) {
            return false;
        }
        if (literals.contains(name)) {
            return input instanceof SymbolExpr other && name.equals(other.name());
        }
        return bindPatternVariable(name, new SinglePatternBinding(input), bindings);
    }

    private static boolean matchRepeatedPattern(
            Expr pattern,
            List<Expr> inputs,
            Set<String> literals,
            Map<String, PatternBinding> bindings
    ) {
        if (pattern instanceof SymbolExpr symbolExpr
                && !"...".equals(symbolExpr.name())
                && !literals.contains(symbolExpr.name())) {
            return bindPatternVariable(
                    symbolExpr.name(),
                    new SequencePatternBinding(List.copyOf(inputs)),
                    bindings
            );
        }
        return false;
    }

    private static boolean bindPatternVariable(
            String name,
            PatternBinding binding,
            Map<String, PatternBinding> bindings
    ) {
        PatternBinding existing = bindings.get(name);
        if (existing != null) {
            return existing.equals(binding);
        }
        bindings.put(name, binding);
        return true;
    }

    private static boolean isEllipsis(Expr expression) {
        return expression instanceof SymbolExpr symbolExpr && "...".equals(symbolExpr.name());
    }

    private static boolean isCoreSyntax(String name) {
        return switch (name) {
            case "and", "or", "begin", "case", "case-lambda", "cond", "define",
                    "define-record-type", "define-syntax", "do", "if", "lambda",
                    "let", "letrec", "letrec*", "quote", "set!", "syntax-rules" -> true;
            default -> false;
        };
    }

    private static String freshMacroName(String name) {
        long counter = MACRO_GENSYM_COUNTER.getAndIncrement();
        StringBuilder sanitized = new StringBuilder();
        for (int index = 0; index < name.length(); index++) {
            char ch = name.charAt(index);
            sanitized.append(Character.isLetterOrDigit(ch) ? ch : '_');
        }
        if (sanitized.isEmpty()) {
            sanitized.append('_');
        }
        return "__macro_" + counter + "_" + sanitized;
    }

    private static EvalError at(SourceLoc loc, String message) {
        return SchemeErrors.at(loc, message);
    }

    private static final class TemplateExpander {
        private final SyntaxRulesMacro definition;
        private final Environment aliasEnv;
        private final Map<String, PatternBinding> bindings;
        private final Map<String, String> renamed;

        private TemplateExpander(
                SyntaxRulesMacro definition,
                Environment aliasEnv,
                Map<String, PatternBinding> bindings
        ) {
            this.definition = definition;
            this.aliasEnv = aliasEnv;
            this.bindings = bindings;
            this.renamed = new HashMap<>();
        }

        private Expr expand(Expr expression) throws EvalError {
            return switch (expression) {
                case NumberExpr numberExpr -> numberExpr;
                case BooleanExpr booleanExpr -> booleanExpr;
                case StringExpr stringExpr -> stringExpr;
                case CharExpr charExpr -> charExpr;
                case SymbolExpr symbolExpr -> expandSymbol(symbolExpr);
                case ListExpr listExpr -> expandList(listExpr);
            };
        }

        private Expr expandSymbol(SymbolExpr symbolExpr) throws EvalError {
            PatternBinding binding = bindings.get(symbolExpr.name());
            if (binding instanceof SinglePatternBinding singleBinding) {
                return singleBinding.value();
            }
            if (binding instanceof SequencePatternBinding) {
                throw at(symbolExpr.loc(),
                        "macro template used repeated variable " + symbolExpr.name()
                                + " without ellipsis");
            }
            return new SymbolExpr(symbolExpr.loc(), hygienicName(symbolExpr.name()));
        }

        private Expr expandList(ListExpr listExpr) throws EvalError {
            List<Expr> expanded = new ArrayList<>(listExpr.elements().size());
            int index = 0;
            while (index < listExpr.elements().size()) {
                if (index + 1 < listExpr.elements().size()
                        && isEllipsis(listExpr.elements().get(index + 1))) {
                    expandRepetition(listExpr.elements().get(index), expanded);
                    index += 2;
                    continue;
                }

                expanded.add(expand(listExpr.elements().get(index)));
                index++;
            }
            return new ListExpr(listExpr.loc(), List.copyOf(expanded));
        }

        private void expandRepetition(Expr expression, List<Expr> expanded) throws EvalError {
            if (expression instanceof SymbolExpr symbolExpr) {
                PatternBinding binding = bindings.get(symbolExpr.name());
                if (binding instanceof SequencePatternBinding sequenceBinding) {
                    expanded.addAll(sequenceBinding.values());
                    return;
                }
                if (binding instanceof SinglePatternBinding) {
                    throw at(symbolExpr.loc(),
                            "macro template repeated non-sequence variable "
                                    + symbolExpr.name());
                }
                throw at(symbolExpr.loc(),
                        "macro template uses ellipsis with non-pattern variable "
                                + symbolExpr.name());
            }

            throw at(expression.loc(),
                    "macro templates only support identifier ellipses at this level");
        }

        private String hygienicName(String name) {
            if ("...".equals(name)
                    || ".".equals(name)
                    || definition.name().equals(name)
                    || isCoreSyntax(name)
                    || definition.env().lookupMacro(name) != null) {
                return name;
            }

            String existing = renamed.get(name);
            if (existing != null) {
                return existing;
            }

            String fresh = freshMacroName(name);
            BindingCell cell = definition.env().lookupCell(name);
            if (cell != null) {
                aliasEnv.defineCell(fresh, cell);
            }
            renamed.put(name, fresh);
            return fresh;
        }
    }
}
