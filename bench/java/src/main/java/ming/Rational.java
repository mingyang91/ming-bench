package ming;

// Exact rational number
class Rational {
    final long num;
    final long den;

    Rational(long num, long den) {
        if (den == 0) throw new ArithmeticException("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        this.num = num / g;
        this.den = den / g;
    }

    boolean isInteger() { return den == 1; }
    Long toLong() { return num; }
    double toDouble() { return (double) num / den; }

    static long gcd(long a, long b) { while (b != 0) { long t = b; b = a % b; a = t; } return a; }

    @Override public boolean equals(Object o) {
        if (o instanceof Rational r) return num == r.num && den == r.den;
        return false;
    }

    @Override public int hashCode() { return Long.hashCode(num) * 31 + Long.hashCode(den); }

    @Override public String toString() { return den == 1 ? Long.toString(num) : num + "/" + den; }
}
