package ming;

import java.util.ArrayList;
import java.util.List;

import static ming.Evaluator.requireLong;
import static ming.SchemeFormatter.schemeToString;

/** Built-in operations on SchemeVector. */
final class VectorBuiltins {

    private VectorBuiltins() {}

    static Object apply(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "vector" -> new SchemeVector(args.toArray());
            case "make-vector" -> {
                int size = (int) requireLong(args.get(0));
                Object fill = args.size() > 1 ? args.get(1) : 0L;
                yield new SchemeVector(size, fill);
            }
            case "vector-ref" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: not a vector");
                yield v.ref((int) requireLong(args.get(1)));
            }
            case "vector-set!" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: not a vector");
                v.set((int) requireLong(args.get(1)), args.get(2));
                yield Evaluator.VOID;
            }
            case "vector-length" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: not a vector");
                yield (long) v.length();
            }
            case "vector?" -> args.get(0) instanceof SchemeVector;
            case "vector->list" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: not a vector");
                Object result = SchemeNil.INSTANCE;
                for (int i = v.length() - 1; i >= 0; i--) result = new SchemePair(v.elements[i], result);
                yield result;
            }
            case "list->vector" -> {
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(0);
                while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                yield new SchemeVector(elems.toArray());
            }
            default -> throw new EvalError("unknown vector operation: " + name);
        };
    }
}
