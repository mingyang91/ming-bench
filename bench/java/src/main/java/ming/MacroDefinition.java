package ming;

import java.util.List;
import java.util.Set;

record MacroDefinition(String name, Set<String> literals, List<SyntaxRule> rules, Environment definingEnvironment) {
    MacroDefinition {
        literals = Set.copyOf(literals);
        rules = List.copyOf(rules);
    }
}

record SyntaxRule(Expr pattern, Expr template) {
}
