package ming;

import java.util.List;

sealed interface Expr permits IntExpr, NumberExpr, BoolExpr, StringExpr, CharExpr, SymbolExpr, ListExpr {
    int line();

    int column();
}

record IntExpr(long value, int line, int column) implements Expr {
}

record NumberExpr(String token, int line, int column) implements Expr {
}

record BoolExpr(boolean value, int line, int column) implements Expr {
}

record StringExpr(String value, int line, int column) implements Expr {
}

record CharExpr(char value, int line, int column) implements Expr {
}

record SymbolExpr(String name, int line, int column) implements Expr {
}

record ListExpr(List<Expr> elements, int line, int column) implements Expr {
}
