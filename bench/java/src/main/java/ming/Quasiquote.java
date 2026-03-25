package ming;

import java.util.List;

class Quasiquote {
    private Quasiquote() {}

    private static Object unwrap(Object expr) {
        if (expr instanceof Token t) return t.value();
        return expr;
    }

    @SuppressWarnings("unchecked")
    static Object expand(Object template) {
        Object raw = unwrap(template);

        if (raw instanceof List<?> list && list.size() == 2 && "unquote".equals(unwrap(list.get(0)))) {
            return list.get(1);
        }

        if (raw instanceof SchemeVector vec) {
            SExpr listForm = new SExpr(null);
            for (Object d : vec.data) listForm.add(d);
            Object expanded = expand(listForm);
            SExpr result = new SExpr(null);
            result.add("list->vector");
            result.add(expanded);
            return result;
        }

        if (!(raw instanceof List<?>)) {
            return quote(template);
        }

        List<?> list = (List<?>) raw;
        if (list.isEmpty()) {
            return quote(template);
        }

        boolean hasSplicing = false;
        for (Object elem : list) {
            Object e = unwrap(elem);
            if (e instanceof List<?> el && el.size() == 2 && "unquote-splicing".equals(unwrap(el.get(0)))) {
                hasSplicing = true;
                break;
            }
        }

        SExpr sexpr = raw instanceof SExpr s ? s : null;
        Object dotTail = sexpr != null ? sexpr.dotTail : null;

        if (!hasSplicing) {
            Object tail = dotTail != null ? expand(dotTail) : quote(new SExpr(null));
            for (int i = list.size() - 1; i >= 0; i--) {
                SExpr consExpr = new SExpr(null);
                consExpr.add("cons");
                consExpr.add(expand(list.get(i)));
                consExpr.add(tail);
                tail = consExpr;
            }
            return tail;
        } else {
            SExpr appendExpr = new SExpr(null);
            appendExpr.add("append");
            for (Object elem : list) {
                Object e = unwrap(elem);
                if (e instanceof List<?> el && el.size() == 2 && "unquote-splicing".equals(unwrap(el.get(0)))) {
                    appendExpr.add(el.get(1));
                } else {
                    SExpr listExpr = new SExpr(null);
                    listExpr.add("list");
                    listExpr.add(expand(elem));
                    appendExpr.add(listExpr);
                }
            }
            if (dotTail != null) {
                appendExpr.add(expand(dotTail));
            }
            return appendExpr;
        }
    }

    static Object quote(Object val) {
        SExpr q = new SExpr(null);
        q.add("quote");
        q.add(val);
        return q;
    }
}
