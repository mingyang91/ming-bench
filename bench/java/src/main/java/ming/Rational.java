package ming;

public record Rational(long numerator, long denominator) {

    public static Rational of(long num, long den) {
        if (den == 0) throw new ArithmeticException("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        return new Rational(num / g, den / g);
    }

    public boolean isInteger() { return denominator == 1; }

    public long toLong() { return numerator; }

    public double toDouble() { return (double) numerator / denominator; }

    public Rational add(Rational other) {
        return of(numerator * other.denominator + other.numerator * denominator,
                  denominator * other.denominator);
    }

    public Rational subtract(Rational other) {
        return of(numerator * other.denominator - other.numerator * denominator,
                  denominator * other.denominator);
    }

    public Rational multiply(Rational other) {
        return of(numerator * other.numerator, denominator * other.denominator);
    }

    public Rational divide(Rational other) {
        return of(numerator * other.denominator, denominator * other.numerator);
    }

    public Rational negate() { return new Rational(-numerator, denominator); }

    @Override
    public String toString() {
        if (denominator == 1) return Long.toString(numerator);
        return numerator + "/" + denominator;
    }

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }
}
