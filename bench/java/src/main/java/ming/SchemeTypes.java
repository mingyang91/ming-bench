package ming;

import java.util.ArrayList;
import java.util.List;

/** Value types used throughout the Scheme interpreter. */
class SchemeTypes {
    private SchemeTypes() {}
}

class SchemeString {
    private final char[] chars;
    private final boolean immutable;
    SchemeString(String value) { this.chars = value.toCharArray(); this.immutable = false; }
    SchemeString(String value, boolean immutable) { this.chars = value.toCharArray(); this.immutable = immutable; }
    SchemeString(char[] chars) { this.chars = chars; this.immutable = false; }
    String value() { return new String(chars); }
    int length() { return chars.length; }
    char charAt(int i) { return chars[i]; }
    boolean isImmutable() { return immutable; }
    void setChar(int i, char c) throws EvalError {
        if (immutable) throw new EvalError("string-set!: strings are immutable");
        chars[i] = c;
    }
}

record SchemeChar(char value) {}

class SchemeRational {
    final long num;
    final long den;
    SchemeRational(long num, long den) {
        if (den == 0) throw new ArithmeticException("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        this.num = num / g;
        this.den = den / g;
    }
    boolean isInteger() { return den == 1; }
    long toLong() { return num / den; }
    double toDouble() { return (double) num / den; }
    private static long gcd(long a, long b) { while (b != 0) { long t = b; b = a % b; a = t; } return a; }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof SchemeRational r)) return false;
        return num == r.num && den == r.den;
    }
    @Override public int hashCode() { return Long.hashCode(num) * 31 + Long.hashCode(den); }
}

record Pos(int line, int col) {
    @Override public String toString() { return line + ":" + col; }
}

class SExpr extends ArrayList<Object> {
    final Pos pos;
    SExpr(Pos pos) { super(); this.pos = pos; }
}

record Token(Object value, Pos pos) {}

class Cons {
    Object car;
    Object cdr;
    Cons(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
}

class SyntaxRules {
    final List<String> literals;
    final List<List<Object>> patterns;
    final List<Object> templates;
    final Evaluator.Env defEnv;
    SyntaxRules(List<String> literals, List<List<Object>> patterns, List<Object> templates, Evaluator.Env defEnv) {
        this.literals = literals;
        this.patterns = patterns;
        this.templates = templates;
        this.defEnv = defEnv;
    }
}

class RecordType {
    final String name;
    final List<String> fieldNames;
    RecordType(String name, List<String> fieldNames) {
        this.name = name;
        this.fieldNames = fieldNames;
    }
}

class SchemeRecord {
    final RecordType type;
    final Object[] fields;
    SchemeRecord(RecordType type, Object[] fields) {
        this.type = type;
        this.fields = fields;
    }
}

class SchemeVector {
    final Object[] data;
    SchemeVector(Object[] data) { this.data = data; }
    int length() { return data.length; }
    Object ref(int i) { return data[i]; }
    void set(int i, Object v) { data[i] = v; }
}

record MacroTransformer(Object proc) {}

record SyntaxOutput(Object form, java.util.Map<String, String> renames, Evaluator.Env defEnv) {}
