package ming;

import java.math.BigInteger;
import java.util.List;

sealed interface Expr permits NumberExpr, BoolExpr, StringExpr, CharExpr, SymbolExpr, ListExpr {
    SourcePos pos();
}

record NumberExpr(SchemeNumber value, SourcePos pos) implements Expr {
}

record BoolExpr(boolean value, SourcePos pos) implements Expr {
}

record StringExpr(String value, SourcePos pos) implements Expr {
}

record CharExpr(char value, SourcePos pos) implements Expr {
}

record SymbolExpr(String name, SourcePos pos, Environment lexicalEnvironment) implements Expr {
    SymbolExpr(String name, SourcePos pos) {
        this(name, pos, null);
    }
}

record ListExpr(List<Expr> elements, SourcePos pos) implements Expr {
}

record SourcePos(int line, int column) {
    @Override
    public String toString() {
        return line + ":" + column;
    }
}
