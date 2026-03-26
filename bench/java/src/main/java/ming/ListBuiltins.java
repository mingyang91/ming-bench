package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * List-related builtin procedures extracted from Evaluator.
 */
final class ListBuiltins {

    @FunctionalInterface
    interface ApplyResolved {
        Object apply(Object proc, List<Object> args) throws EvalError, ContinuationException, SchemeRaiseException;
    }

    static Object apply(String name, List<Object> args, ApplyResolved applyResolved) throws EvalError, ContinuationException, SchemeRaiseException {
        return switch (name) {
            case "cons" -> new SchemePair(args.get(0), args.get(1));
            case "car" -> {
                if (args.get(0) instanceof SchemePair p) yield p.car;
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                if (args.get(0) instanceof SchemePair p) yield p.cdr;
                throw new EvalError("cdr: not a pair");
            }
            case "null?" -> args.get(0) instanceof SchemeNil;
            case "list" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) result = new SchemePair(args.get(i), result);
                yield result;
            }
            case "length" -> {
                Object val = args.get(0);
                Object slow = val;
                long len = 0;
                while (val instanceof SchemePair p) {
                    len++;
                    val = p.cdr;
                    if (len % 2 == 0 && slow instanceof SchemePair sp) slow = sp.cdr;
                    if (val == slow && len > 0 && val instanceof SchemePair) throw new EvalError("length: not a proper list");
                }
                if (!(val instanceof SchemeNil)) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                Object result = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (lst instanceof SchemeNil) continue;
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
                        List<Object> elems = new ArrayList<>();
                        Object cur = lst;
                        while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                        for (int j = elems.size() - 1; j >= 0; j--) result = new SchemePair(elems.get(j), result);
                    }
                }
                yield result;
            }
            case "list-ref" -> {
                Object lst = args.get(0);
                int idx = (int) Evaluator.requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
                    lst = p.cdr;
                }
                if (!(lst instanceof SchemePair p)) throw new EvalError("list-ref: index out of range");
                yield p.car;
            }
            case "list-tail" -> {
                Object lst = args.get(0);
                int idx = (int) Evaluator.requireLong(args.get(1));
                for (int i = 0; i < idx; i++) {
                    if (!(lst instanceof SchemePair p)) throw new EvalError("list-tail: index out of range");
                    lst = p.cdr;
                }
                yield lst;
            }
            case "list?" -> {
                Object slow = args.get(0);
                Object fast = args.get(0);
                while (fast instanceof SchemePair fp) {
                    fast = fp.cdr;
                    if (fast instanceof SchemeNil) { yield true; }
                    if (!(fast instanceof SchemePair)) { yield false; }
                    fast = ((SchemePair) fast).cdr;
                    slow = ((SchemePair) slow).cdr;
                    if (slow == fast) { yield false; }
                }
                yield fast instanceof SchemeNil;
            }
            case "assoc" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (p.car instanceof SchemePair entry) {
                        if (Evaluator.schemeEqual(key, entry.car)) yield entry;
                    }
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "map" -> {
                if (args.size() < 2) throw new EvalError("map: expected at least 2 arguments");
                Object proc = args.get(0);
                List<List<Object>> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) {
                    List<Object> elems = new ArrayList<>();
                    Object cur = args.get(i);
                    while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                    lists.add(elems);
                }
                int len = lists.get(0).size();
                List<Object> results = new ArrayList<>();
                for (int i = 0; i < len; i++) {
                    List<Object> callArgs = new ArrayList<>();
                    for (List<Object> l : lists) callArgs.add(l.get(i));
                    results.add(applyResolved.apply(proc, callArgs));
                }
                Object result = SchemeNil.INSTANCE;
                for (int i = results.size() - 1; i >= 0; i--) result = new SchemePair(results.get(i), result);
                yield result;
            }
            case "set-car!" -> {
                if (!(args.get(0) instanceof SchemePair p)) throw new EvalError("set-car!: not a pair");
                p.car = args.get(1);
                yield Evaluator.VOID;
            }
            case "set-cdr!" -> {
                if (!(args.get(0) instanceof SchemePair p)) throw new EvalError("set-cdr!: not a pair");
                p.cdr = args.get(1);
                yield Evaluator.VOID;
            }
            case "for-each" -> {
                if (args.size() < 2) throw new EvalError("for-each: expected at least 2 arguments");
                Object proc = args.get(0);
                List<List<Object>> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) {
                    List<Object> elems = new ArrayList<>();
                    Object cur = args.get(i);
                    while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                    lists.add(elems);
                }
                int len = lists.get(0).size();
                for (int i = 0; i < len; i++) {
                    List<Object> callArgs = new ArrayList<>();
                    for (List<Object> l : lists) callArgs.add(l.get(i));
                    applyResolved.apply(proc, callArgs);
                }
                yield Evaluator.VOID;
            }
            case "caar", "cadr", "cdar", "cddr", "caddr", "cadar", "caddar",
                 "caaar", "caadr", "cdaar", "cdadr", "cddar", "cdddr",
                 "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
                 "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr" -> {
                yield applyCxr(name, args.get(0));
            }
            case "reverse" -> {
                Object lst = args.get(0);
                Object result = SchemeNil.INSTANCE;
                while (lst instanceof SchemePair p) {
                    result = new SchemePair(p.car, result);
                    lst = p.cdr;
                }
                yield result;
            }
            case "memq" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (Evaluator.schemeEqv(key, p.car)) yield lst;
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "memv" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (Evaluator.schemeEqv(key, p.car)) yield lst;
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "member" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (Evaluator.schemeEqual(key, p.car)) yield lst;
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "assq" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (p.car instanceof SchemePair entry) {
                        Object k = entry.car;
                        if (k == key || k.equals(key) ||
                            (k instanceof SchemeSymbol sk && key instanceof SchemeSymbol sy && sk.name().equals(sy.name()))) {
                            yield entry;
                        }
                    }
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            case "assv" -> {
                Object key = args.get(0);
                Object lst = args.get(1);
                while (lst instanceof SchemePair p) {
                    if (p.car instanceof SchemePair entry && Evaluator.schemeEqv(key, entry.car)) yield entry;
                    lst = p.cdr;
                }
                yield Boolean.FALSE;
            }
            default -> throw new EvalError("unknown list procedure: " + name);
        };
    }

    private static Object applyCxr(String name, Object val) throws EvalError {
        for (int i = name.length() - 2; i >= 1; i--) {
            if (!(val instanceof SchemePair p)) throw new EvalError(name + ": not a pair");
            val = name.charAt(i) == 'a' ? p.car : p.cdr;
        }
        return val;
    }

    private ListBuiltins() {}
}
