package ming;

import java.util.List;

sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
    int line();

    int column();
}

record IntExpr(long value, int line, int column) implements Expr {
}

record BoolExpr(boolean value, int line, int column) implements Expr {
}

record StringExpr(String value, int line, int column) implements Expr {
}

record SymbolExpr(String name, int line, int column) implements Expr {
}

record ListExpr(List<Expr> elements, int line, int column) implements Expr {
}
