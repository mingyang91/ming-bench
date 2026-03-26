package ming;

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

    long toLong() { return num; }

    double toDouble() { return (double) num / den; }

    Rational add(Rational o) {
        return new Rational(num * o.den + o.num * den, den * o.den);
    }

    Rational sub(Rational o) {
        return new Rational(num * o.den - o.num * den, den * o.den);
    }

    Rational mul(Rational o) {
        return new Rational(num * o.num, den * o.den);
    }

    Rational div(Rational o) {
        return new Rational(num * o.den, den * o.num);
    }

    Rational negate() {
        return new Rational(-num, den);
    }

    /** Returns Long if integer, otherwise Rational */
    Object simplify() {
        return den == 1 ? num : this;
    }

    @Override
    public String toString() {
        return num + "/" + den;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Rational r)) return false;
        return num == r.num && den == r.den;
    }

    @Override
    public int hashCode() {
        return Long.hashCode(num) * 31 + Long.hashCode(den);
    }

    static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    static Rational fromLong(long n) {
        return new Rational(n, 1);
    }

    static Rational fromDouble(double d) {
        // Convert double to rational using continued fraction approximation
        if (d == Math.floor(d) && !Double.isInfinite(d)) {
            return new Rational((long) d, 1);
        }
        // Use the fact that d = n/d where we can represent via long fraction
        // Simple approach: multiply by power of 2 from the double's representation
        long bits = Double.doubleToLongBits(d);
        int exponent = (int) ((bits >> 52) & 0x7FF) - 1023;
        long mantissa = (bits & 0x000FFFFFFFFFFFFFL) | 0x0010000000000000L;
        boolean negative = (bits & 0x8000000000000000L) != 0;

        // d = mantissa * 2^(exponent - 52)
        int shift = exponent - 52;
        long num, den;
        if (shift >= 0) {
            num = mantissa << shift;
            den = 1;
        } else {
            num = mantissa;
            den = 1L << (-shift);
        }
        if (negative) num = -num;
        return new Rational(num, den);
    }

    int compareTo(Rational o) {
        return Long.compare(num * o.den, o.num * den);
    }
}
