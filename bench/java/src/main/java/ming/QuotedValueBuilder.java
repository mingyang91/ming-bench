package ming;

import java.util.List;

final class QuotedValueBuilder {
    private QuotedValueBuilder() {
    }

    static Value quote(Expr expression) throws EvalError {
        if (expression instanceof NumberExpr numberExpr) {
            return numberExpr.value().toValue();
        }
        if (expression instanceof BoolExpr boolExpr) {
            return BoolValue.of(boolExpr.value());
        }
        if (expression instanceof StringExpr stringExpr) {
            return new StringValue(stringExpr.value());
        }
        if (expression instanceof CharExpr charExpr) {
            return new CharValue(charExpr.value());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            return new SymbolValue(symbolExpr.name());
        }
        if (expression instanceof ListExpr listExpr) {
            return quoteList(listExpr.elements());
        }
        throw new EvalError("unsupported quoted expression", expression.pos().line(),
                expression.pos().column());
    }

    private static Value quoteList(List<Expr> expressions) throws EvalError {
        Value result = EmptyListValue.INSTANCE;
        for (int i = expressions.size() - 1; i >= 0; i--) {
            result = new PairValue(quote(expressions.get(i)), result);
        }
        return result;
    }
}
