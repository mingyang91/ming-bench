package ming;

import java.util.List;
import java.util.Set;

record MacroDefinition(String name,
                       Set<String> literals,
                       List<SyntaxRule> rules,
                       Environment definingEnvironment,
                       ProcedureValue transformer) {
    MacroDefinition {
        literals = literals == null ? Set.of() : Set.copyOf(literals);
        rules = rules == null ? List.of() : List.copyOf(rules);
    }

    static MacroDefinition syntaxRules(String name,
                                       Set<String> literals,
                                       List<SyntaxRule> rules,
                                       Environment definingEnvironment) {
        return new MacroDefinition(name, literals, rules, definingEnvironment, null);
    }

    static MacroDefinition transformer(String name,
                                       ProcedureValue transformer,
                                       Environment definingEnvironment) {
        return new MacroDefinition(name, Set.of(), List.of(), definingEnvironment, transformer);
    }

    boolean isSyntaxRules() {
        return transformer == null;
    }
}

record SyntaxRule(Expr pattern, Expr template) {
}
