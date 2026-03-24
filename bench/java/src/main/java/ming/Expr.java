package ming;

import java.util.List;

sealed interface Expr permits Expr.IntegerExpr, Expr.BooleanExpr, Expr.StringExpr, Expr.SymbolExpr, Expr.ListExpr {
    SourcePos pos();

    record IntegerExpr(long value, SourcePos pos) implements Expr {}

    record BooleanExpr(boolean value, SourcePos pos) implements Expr {}

    record StringExpr(String value, SourcePos pos) implements Expr {}

    record SymbolExpr(String name, SourcePos pos) implements Expr {}

    record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {}
}
