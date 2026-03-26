package ming;

public class SchemeRational {
    public final long numerator;
    public final long denominator;

    private SchemeRational(long num, long den) {
        this.numerator = num;
        this.denominator = den;
    }

    public static Object make(long num, long den) {
        if (den == 0) throw new ArithmeticException("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        num /= g;
        den /= g;
        if (den == 1) return num; // simplify to integer
        return new SchemeRational(num, den);
    }

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    public double toDouble() {
        return (double) numerator / denominator;
    }

    @Override
    public String toString() {
        return numerator + "/" + denominator;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o instanceof SchemeRational r) return numerator == r.numerator && denominator == r.denominator;
        return false;
    }

    @Override
    public int hashCode() {
        return Long.hashCode(numerator) * 31 + Long.hashCode(denominator);
    }
}
