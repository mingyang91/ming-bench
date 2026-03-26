package ming;

import java.util.List;

sealed interface Expr permits IntExpr, RationalExpr, InexactExpr, BoolExpr, CharExpr,
        StringExpr, SymbolExpr, ListExpr, VectorExpr {
    int line();

    int column();
}

record IntExpr(long value, int line, int column) implements Expr {
}

record RationalExpr(long numerator, long denominator, int line, int column) implements Expr {
}

record InexactExpr(double value, int line, int column) implements Expr {
}

record BoolExpr(boolean value, int line, int column) implements Expr {
}

record CharExpr(char value, int line, int column) implements Expr {
}

record StringExpr(String value, int line, int column) implements Expr {
}

record SymbolExpr(String name, int line, int column) implements Expr {
}

record ListExpr(List<Expr> elements, int line, int column) implements Expr {
}

record VectorExpr(List<Expr> elements, int line, int column) implements Expr {
}
