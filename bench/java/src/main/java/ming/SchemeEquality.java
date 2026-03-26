package ming;

import static ming.Evaluator.requireString;

/** Equality predicates: eq?, eqv?, equal? */
final class SchemeEquality {

    private SchemeEquality() {}

    static boolean schemeEqv(Object a, Object b) {
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (ArithmeticOps.isNumber(a) && ArithmeticOps.isNumber(b)) {
            try { return ArithmeticOps.toDouble(a) == ArithmeticOps.toDouble(b); } catch (EvalError e) { return false; }
        }
        return a == b;
    }

    static boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, 0);
    }

    static boolean schemeEqualRec(Object a, Object b, int depth) {
        if (a == b) return true;
        if (depth > 100000) return false;
        if (a instanceof SchemePair pa && b instanceof SchemePair pb) {
            return schemeEqualRec(pa.car, pb.car, depth + 1) && schemeEqualRec(pa.cdr, pb.cdr, depth + 1);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualRec(va.elements[i], vb.elements[i], depth + 1)) return false;
            }
            return true;
        }
        if (a instanceof SchemeNil && b instanceof SchemeNil) return true;
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (ArithmeticOps.isNumber(a) && ArithmeticOps.isNumber(b)) {
            try { return ArithmeticOps.toDouble(a) == ArithmeticOps.toDouble(b); } catch (EvalError e) { return false; }
        }
        if ((a instanceof String || a instanceof SchemeString) && (b instanceof String || b instanceof SchemeString)) {
            try { return requireString(a).equals(requireString(b)); } catch (EvalError e) { return false; }
        }
        if (a == null || b == null) return a == b;
        return a.equals(b);
    }
}
