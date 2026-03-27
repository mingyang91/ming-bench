package ming;

import java.util.List;
import java.util.Set;

record MacroRule(List<Expr> pattern, Expr template) { }

record SyntaxRulesMacro(String name, Set<String> literals, List<MacroRule> rules, Environment env) { }

sealed interface PatternBinding permits SinglePatternBinding, SequencePatternBinding { }

record SinglePatternBinding(Expr value) implements PatternBinding { }

record SequencePatternBinding(List<Expr> values) implements PatternBinding { }

record MacroExpansion(Expr expression, Environment environment) { }
