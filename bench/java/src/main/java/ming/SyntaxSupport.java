package ming;

import java.util.HashMap;
import java.util.Map;

final class SyntaxValue implements SchemeValue {
    private final SchemeExpression expression;
    private final SyntaxContext context;

    SyntaxValue(SchemeExpression expression, SyntaxContext context) {
        this.expression = expression;
        this.context = context;
    }

    SchemeExpression expression() {
        return expression;
    }

    SyntaxContext context() {
        return context;
    }

    @Override
    public String render() {
        return "#<syntax>";
    }
}

final class PatternBindingValue implements SchemeValue {
    private final SyntaxMatcher.PatternBinding binding;

    PatternBindingValue(SyntaxMatcher.PatternBinding binding) {
        this.binding = binding;
    }

    SyntaxMatcher.PatternBinding binding() {
        return binding;
    }

    @Override
    public String render() {
        return "#<syntax-binding>";
    }
}

sealed interface SyntaxContext permits SyntaxTemplateContext, UseSiteSyntaxContext {
    SchemeExpression contextualize(SchemeExpression expression);
}

enum UseSiteSyntaxContext implements SyntaxContext {
    INSTANCE;

    @Override
    public SchemeExpression contextualize(SchemeExpression expression) {
        return expression;
    }
}

final class SyntaxTemplateContext implements SyntaxContext {
    private static long nextTemplateId = 1L;

    private final long templateId;
    private final Environment definitionEnvironment;
    private final Map<String, String> identifierAliases = new HashMap<>();

    SyntaxTemplateContext(Environment definitionEnvironment) {
        this.templateId = nextTemplateId++;
        this.definitionEnvironment = definitionEnvironment;
    }

    @Override
    public SchemeExpression contextualize(SchemeExpression expression) {
        return SyntaxMatcher.contextualize(expression, this);
    }

    SymbolExpression aliasSymbol(SymbolExpression symbol) {
        return new SymbolExpression(aliasFor(symbol.name()), symbol.position());
    }

    String aliasFor(String name) {
        return identifierAliases.computeIfAbsent(name, key -> {
            String alias = "__macro$" + templateId + "$" + key;
            definitionEnvironment.defineAlias(alias, key);
            return alias;
        });
    }
}
