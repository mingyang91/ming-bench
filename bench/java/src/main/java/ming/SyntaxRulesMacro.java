package ming;

import java.util.List;

class SyntaxRulesMacro {
    final String name;
    final List<String> literals;
    final List<Object[]> rules; // each: [pattern, template]
    final Evaluator.Env defEnv;
    SyntaxRulesMacro(String name, List<String> literals, List<Object[]> rules, Evaluator.Env defEnv) {
        this.name = name;
        this.literals = literals;
        this.rules = rules;
        this.defEnv = defEnv;
    }
}
