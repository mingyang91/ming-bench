package ming;

import java.util.List;

sealed interface SchemeExpression
        permits LiteralExpression, SymbolExpression, ListExpression {
    SourcePosition position();
}

record LiteralExpression(SchemeValue value, SourcePosition position) implements SchemeExpression {
}

record SymbolExpression(String name, SourcePosition position) implements SchemeExpression {
}

record ListExpression(List<SchemeExpression> elements, SourcePosition position) implements SchemeExpression {
}
