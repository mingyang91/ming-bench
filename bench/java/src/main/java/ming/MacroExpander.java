package ming;

import static ming.Evaluator.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

class MacroExpander {

    private int gensymCounter = 0;

    int nextGensym() {
        return gensymCounter++;
    }

    @SuppressWarnings("unchecked")
    Object[] expandMacro(SyntaxRulesMacro macro, List<?> form, Env useEnv) throws EvalError {
        for (Object[] rule : macro.rules) {
            Map<String, Object> bindings = matchPattern(rule[0], form, macro.literals);
            if (bindings != null) {
                Map<String, String> renameMap = new HashMap<>();
                Object expanded = expandTemplate(rule[1], bindings, renameMap);
                if (!renameMap.isEmpty()) {
                    Map<String, Object> hb = new HashMap<>();
                    for (var e : renameMap.entrySet()) {
                        try { hb.put(e.getValue(), macro.defEnv.lookup(e.getKey(), new SchemeParser.Pos(0, 0))); }
                        catch (EvalError ignored) {}
                    }
                    if (!hb.isEmpty()) { for (var e : hb.entrySet()) useEnv.define(e.getKey(), e.getValue()); }
                }
                return new Object[]{expanded, useEnv};
            }
        }
        throw new EvalError("no matching pattern for macro " + macro.name);
    }

    Map<String, Object> matchPattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof SchemeParser.Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1;
        int patEnd = patList.size();
        boolean hasDottedTail = false;
        Object dottedVar = null;
        if (patEnd > 0 && patList.get(patEnd - 1) instanceof SchemeParser.DottedTail dt) {
            hasDottedTail = true;
            dottedVar = dt.expr();
            if (dottedVar instanceof SchemeParser.Located loc) dottedVar = loc.expr;
            patEnd--;
        }
        while (pi < patEnd) {
            Object pe = patList.get(pi); if (pe instanceof SchemeParser.Located loc) pe = loc.expr;
            boolean hasE = false;
            if (pi + 1 < patEnd) {
                Object nx = patList.get(pi + 1); if (nx instanceof SchemeParser.Located loc) nx = loc.expr;
                if ("...".equals(nx)) hasE = true;
            }
            if (hasE) {
                if (!(pe instanceof String vn)) return null;
                List<Object> coll = new ArrayList<>();
                while (ii < input.size()) { coll.add(input.get(ii)); ii++; }
                bindings.put(vn, coll); pi += 2;
            } else if (pe instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object ie = input.get(ii); if (ie instanceof SchemeParser.Located loc) ie = loc.expr;
                if (!s.equals(ie)) return null;
                pi++; ii++;
            } else if (pe instanceof String vn) {
                if (ii >= input.size()) return null;
                bindings.put(vn, input.get(ii)); pi++; ii++;
            } else return null;
        }
        if (hasDottedTail) {
            if (dottedVar instanceof String vn) {
                Object rest = EMPTY_LIST;
                for (int ri = input.size() - 1; ri >= ii; ri--) rest = new Pair(input.get(ri), rest);
                bindings.put(vn, rest);
            }
            return bindings;
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    Object expandTemplate(Object template, Map<String, Object> bindings, Map<String, String> renameMap) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String id) {
            if (bindings.containsKey(id)) return bindings.get(id);
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            return renameMap.get(id);
        }
        if (template instanceof SchemeParser.DottedTail dt) {
            return expandTemplate(dt.expr(), bindings, renameMap);
        }
        if (template instanceof List<?> tl) {
            List<Object> result = new ArrayList<>();
            int end = tl.size();
            SchemeParser.DottedTail dottedTail = null;
            if (end > 0 && tl.get(end - 1) instanceof SchemeParser.DottedTail dt) {
                dottedTail = dt;
                end--;
            }
            for (int i = 0; i < end; i++) {
                Object elem = tl.get(i);
                Object raw = elem; if (raw instanceof SchemeParser.Located loc) raw = loc.expr;
                boolean nxt = false;
                if (i + 1 < end) { Object n = tl.get(i + 1); if (n instanceof SchemeParser.Located loc) n = loc.expr; if ("...".equals(n)) nxt = true; }
                if (nxt) {
                    String ev = findEllipsisVar(elem, bindings);
                    if (ev != null) {
                        for (Object e : (List<Object>) bindings.get(ev)) {
                            Map<String, Object> sb = new HashMap<>(bindings); sb.put(ev, e);
                            result.add(expandTemplate(elem, sb, renameMap));
                        }
                    }
                    i++;
                } else if (raw instanceof String s && "...".equals(s)) { /* skip */ }
                else result.add(expandTemplate(elem, bindings, renameMap));
            }
            if (dottedTail != null) {
                Object tail = expandTemplate(dottedTail.expr(), bindings, renameMap);
                while (tail instanceof Pair p) { result.add(p.car); tail = p.cdr; }
                if (tail != EMPTY_LIST && tail != null) {
                    result.add(new SchemeParser.DottedTail(tail));
                }
            }
            return result;
        }
        return template;
    }

    String findEllipsisVar(Object template, Map<String, Object> bindings) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String s && bindings.get(s) instanceof List) return s;
        if (template instanceof List<?> list) {
            for (Object e : list) { String f = findEllipsisVar(e, bindings); if (f != null) return f; }
        }
        return null;
    }

    Map<String, Object> matchSyntaxCasePattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof SchemeParser.Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 0, ii = 0;
        while (pi < patList.size()) {
            Object pe = patList.get(pi);
            if (pe instanceof SchemeParser.Located loc) pe = loc.expr;
            boolean hasE = false;
            if (pi + 1 < patList.size()) {
                Object nx = patList.get(pi + 1);
                if (nx instanceof SchemeParser.Located loc) nx = loc.expr;
                if ("...".equals(nx)) hasE = true;
            }
            if (hasE) {
                if (!(pe instanceof String vn)) return null;
                List<Object> coll = new ArrayList<>();
                while (ii < input.size()) { coll.add(input.get(ii)); ii++; }
                bindings.put(vn, coll); pi += 2;
            } else if (pe instanceof String s && s.equals("_")) {
                if (ii >= input.size()) return null;
                pi++; ii++;
            } else if (pe instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object ie = input.get(ii);
                if (ie instanceof SchemeParser.Located loc) ie = loc.expr;
                if (!s.equals(ie)) return null;
                pi++; ii++;
            } else if (pe instanceof String vn) {
                if (ii >= input.size()) return null;
                bindings.put(vn, input.get(ii)); pi++; ii++;
            } else return null;
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    Object expandSyntaxTemplate(Object template, Env env,
                                Map<String, String> renameMap,
                                Map<String, Object> hygieneBindings,
                                Env syntaxDefEnv) {
        template = unwrap(template);
        if (template instanceof String id) {
            Object val = null;
            try { val = env.lookup(id, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
            if (val instanceof SyntaxObject so) return so.datum;
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            String renamed = renameMap.get(id);
            if (syntaxDefEnv != null && !hygieneBindings.containsKey(renamed)) {
                try {
                    hygieneBindings.put(renamed, syntaxDefEnv.lookup(id, new SchemeParser.Pos(0, 0)));
                } catch (EvalError ignored) {}
            }
            return renamed;
        }
        if (template instanceof List<?> tl) {
            if (!tl.isEmpty()) {
                Object head = unwrap(tl.get(0));
                if ("quote".equals(head)) return tl;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tl.size(); i++) {
                Object elem = tl.get(i);
                Object raw = unwrap(elem);
                boolean hasEllipsis = false;
                if (i + 1 < tl.size()) {
                    Object nx = unwrap(tl.get(i + 1));
                    if ("...".equals(nx)) hasEllipsis = true;
                }
                if (hasEllipsis) {
                    String ev = findSyntaxEllipsisVar(elem, env);
                    if (ev != null) {
                        Object evVal = null;
                        try { evVal = env.lookup(ev, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
                        if (evVal instanceof SyntaxObject so && so.datum instanceof List<?> items) {
                            for (Object item : items) {
                                Env subEnv = new Env(env);
                                subEnv.define(ev, new SyntaxObject(item));
                                result.add(expandSyntaxTemplate(elem, subEnv, renameMap, hygieneBindings, syntaxDefEnv));
                            }
                        }
                    }
                    i++;
                } else if ("...".equals(raw)) {
                    // skip
                } else {
                    result.add(expandSyntaxTemplate(elem, env, renameMap, hygieneBindings, syntaxDefEnv));
                }
            }
            return result;
        }
        return template;
    }

    String findSyntaxEllipsisVar(Object template, Env env) {
        template = unwrap(template);
        if (template instanceof String id) {
            Object val = null;
            try { val = env.lookup(id, new SchemeParser.Pos(0, 0)); } catch (EvalError ignored) {}
            if (val instanceof SyntaxObject so && so.datum instanceof List) return id;
            return null;
        }
        if (template instanceof List<?> tl) {
            for (Object e : tl) {
                String found = findSyntaxEllipsisVar(e, env);
                if (found != null) return found;
            }
        }
        return null;
    }

    private static Object unwrap(Object obj) {
        return Evaluator.unwrap(obj);
    }
}
