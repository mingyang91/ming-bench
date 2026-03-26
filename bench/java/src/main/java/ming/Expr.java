package ming;

import java.util.List;

sealed interface Expr permits IntExpr, BoolExpr, StringExpr, SymbolExpr, ListExpr {
}

record IntExpr(long value) implements Expr {
}

record BoolExpr(boolean value) implements Expr {
}

record StringExpr(String value) implements Expr {
}

record SymbolExpr(String name) implements Expr {
}

record ListExpr(List<Expr> elements) implements Expr {
}
