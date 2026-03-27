package ming;

import java.util.List;

record SourceLoc(int line, int column) { }

record BindingParseResult(List<String> names, List<Value> values) { }

record ParameterSpec(List<String> requiredParameters, String restParameter) { }

sealed interface Expr permits NumberExpr, BooleanExpr, StringExpr, CharExpr, SymbolExpr, ListExpr {
    SourceLoc loc();
}

record NumberExpr(SourceLoc loc, SchemeNumber value) implements Expr { }

record BooleanExpr(SourceLoc loc, boolean value) implements Expr { }

record StringExpr(SourceLoc loc, String value) implements Expr { }

record CharExpr(SourceLoc loc, int codePoint) implements Expr { }

record SymbolExpr(SourceLoc loc, String name) implements Expr { }

record ListExpr(SourceLoc loc, List<Expr> elements) implements Expr { }
