package ming;

import java.util.List;

sealed interface SchemeExpression
        permits LiteralExpression, SymbolExpression, ListExpression {
}

record LiteralExpression(SchemeValue value) implements SchemeExpression {
}

record SymbolExpression(String name) implements SchemeExpression {
}

record ListExpression(List<SchemeExpression> elements) implements SchemeExpression {
}
