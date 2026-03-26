package ming;

import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

class SyntaxRules {
    final List<String> literals;
    final List<Rule> rules;
    final Env defEnv;

    private static final AtomicLong counter = new AtomicLong(0);

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "define", "if", "quote", "lambda", "set!", "and", "or", "begin",
        "let", "let*", "letrec", "letrec*", "cond", "case",
        "define-syntax", "syntax-rules", "syntax-case", "syntax", "with-syntax",
        "define-record-type", "case-lambda", "do", "guard",
        "when", "unless", "raise", "values", "call-with-values",
        "call/cc", "call-with-current-continuation", "dynamic-wind",
        "quasiquote", "unquote", "unquote-splicing"
    );

    SyntaxRules(List<String> literals, List<Rule> rules, Env defEnv) {
        this.literals = literals;
        this.rules = rules;
        this.defEnv = defEnv;
    }

    static class Rule {
        final Object pattern; // List or Pair
        final Object template;
        Rule(Object pattern, Object template) {
            this.pattern = pattern;
            this.template = template;
        }
    }

    // Flatten a pattern/template (List or Pair chain) into elements + optional rest
    // Returns: [restVar_or_null, elem0, elem1, ...]
    private static Object[] flatten(Object obj) {
        if (obj instanceof List<?> l) {
            Object[] r = new Object[l.size() + 1];
            r[0] = null;
            for (int i = 0; i < l.size(); i++) r[i + 1] = l.get(i);
            return r;
        }
        if (obj instanceof Pair) {
            List<Object> elems = new ArrayList<>();
            Object cur = obj;
            while (cur instanceof Pair p) {
                elems.add(p.car);
                cur = p.cdr;
            }
            Object rest = (cur == SchemeValue.NIL || cur == null) ? null : cur;
            Object[] r = new Object[elems.size() + 1];
            r[0] = rest;
            for (int i = 0; i < elems.size(); i++) r[i + 1] = elems.get(i);
            return r;
        }
        return new Object[] { null, obj };
    }

    static SyntaxRules parse(List<?> form, Env defEnv) throws EvalError {
        if (form.size() < 3) throw new EvalError("syntax-rules: bad syntax");
        List<String> literals = new ArrayList<>();
        if (form.get(1) instanceof List<?> litList) {
            for (Object l : litList) {
                if (l instanceof String s) literals.add(s);
            }
        }
        List<Rule> rules = new ArrayList<>();
        for (int i = 2; i < form.size(); i++) {
            if (!(form.get(i) instanceof List<?> clause) || clause.size() != 2)
                throw new EvalError("syntax-rules: bad clause");
            Object pattern = clause.get(0);
            if (!(pattern instanceof List<?>) && !(pattern instanceof Pair))
                throw new EvalError("syntax-rules: pattern must be a list");
            rules.add(new Rule(pattern, clause.get(1)));
        }
        return new SyntaxRules(literals, rules, defEnv);
    }

    Object expand(List<?> form, Env callEnv) throws EvalError {
        Object[] result = expandToForm(form, callEnv);
        return Evaluator.eval(result[0], (Env) result[1]);
    }

    Object[] expandToForm(List<?> form, Env callEnv) throws EvalError {
        for (Rule rule : rules) {
            Map<String, Object> bindings = new HashMap<>();
            Map<String, List<Object>> ellipsisBindings = new HashMap<>();
            if (matchPattern(rule.pattern, form, bindings, ellipsisBindings)) {
                Set<String> patternVars = new HashSet<>(bindings.keySet());
                patternVars.addAll(ellipsisBindings.keySet());
                Map<String, String> renames = new HashMap<>();
                Object expanded = expandTemplate(rule.template, bindings, ellipsisBindings, patternVars, renames);

                for (var entry : renames.entrySet()) {
                    try {
                        Object val = defEnv.lookup(entry.getKey());
                        callEnv.define(entry.getValue(), val);
                    } catch (EvalError e) { /* not bound at def site, skip */ }
                }

                return new Object[] { expanded, callEnv };
            }
        }
        throw new EvalError("syntax-rules: no matching pattern for " + SchemeValue.toStr(form));
    }

    private boolean matchPattern(Object pattern, List<?> form,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings) {
        Object[] flat = flatten(pattern);
        Object restVar = flat[0]; // null or String for dotted rest
        int patLen = flat.length - 1; // number of pattern elements

        int pi = 1, fi = 1; // skip macro name in both pattern and form (index 0)
        while (pi < patLen) {
            Object pe = flat[pi + 1]; // +1 because flat[0] is restVar
            boolean hasEllipsis = pi + 1 < patLen && flat[pi + 2] instanceof String ds && "...".equals(ds);

            if (hasEllipsis) {
                // Calculate how many remaining non-ellipsis pattern elements follow
                int remaining = 0;
                for (int j = pi + 2; j < patLen; j++) {
                    if (!(flat[j + 1] instanceof String ss && "...".equals(ss))) remaining++;
                }
                int available = form.size() - fi - remaining;
                if (available < 0) return false;

                if (pe instanceof String varName && !varName.startsWith("\"")) {
                    List<Object> collected = new ArrayList<>();
                    for (int i = 0; i < available; i++) collected.add(form.get(fi + i));
                    ellipsisBindings.put(varName, collected);
                } else {
                    // Sub-pattern with ellipsis — each form element must match the sub-pattern
                    for (int i = 0; i < available; i++) {
                        if (!matchSubPattern(pe, form.get(fi + i), bindings, ellipsisBindings))
                            return false;
                    }
                }
                fi += available;
                pi += 2;
            } else {
                if (fi >= form.size()) return false;
                if (!matchSubPattern(pe, form.get(fi), bindings, ellipsisBindings))
                    return false;
                pi++;
                fi++;
            }
        }

        // Handle dotted rest variable
        if (restVar instanceof String rv) {
            // Bind remaining form elements as a Scheme list
            Object rest = SchemeValue.NIL;
            for (int i = form.size() - 1; i >= fi; i--) {
                rest = new Pair(form.get(i), rest);
            }
            bindings.put(rv, rest);
            return true;
        }

        return fi == form.size();
    }

    // Match a single pattern element against a form element (handles nested lists/pairs)
    private boolean matchSubPattern(Object pe, Object fe,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings) {
        if (pe instanceof String s && !s.startsWith("\"")) {
            if ("_".equals(s)) return true; // wildcard
            if (literals.contains(s)) {
                return s.equals(fe);
            }
            bindings.put(s, fe);
            return true;
        }
        // Nested list/pair pattern
        if ((pe instanceof List<?> || pe instanceof Pair) && fe instanceof List<?> feList) {
            return matchPattern(pe, feList, bindings, ellipsisBindings);
        }
        if ((pe instanceof List<?> || pe instanceof Pair) && fe instanceof Pair) {
            // Convert Pair form element to a list for matching
            List<Object> feList = new ArrayList<>();
            Object cur = fe;
            while (cur instanceof Pair p) { feList.add(p.car); cur = p.cdr; }
            if (cur != SchemeValue.NIL && cur != null) return false; // improper list
            return matchPattern(pe, feList, bindings, ellipsisBindings);
        }
        return Objects.equals(pe, fe);
    }

    private Object expandTemplate(Object tmpl, Map<String, Object> bindings,
            Map<String, List<Object>> ellipsisBindings, Set<String> patternVars,
            Map<String, String> renames) throws EvalError {
        if (tmpl instanceof String s) {
            if (s.startsWith("\"")) return s;
            if (patternVars.contains(s) && bindings.containsKey(s)) return bindings.get(s);
            if (!SPECIAL_FORMS.contains(s) && !patternVars.contains(s) && !"...".equals(s)) {
                return renames.computeIfAbsent(s, k -> k + "$" + counter.incrementAndGet());
            }
            return s;
        }
        if (tmpl instanceof Pair) {
            // Dotted template like (begin . exps) or (cond (test body) . rest)
            // Flatten, expand elements, splice rest binding
            Object[] flat = flatten(tmpl);
            Object restTmpl = flat[0]; // the dotted rest part
            List<Object> result = new ArrayList<>();
            for (int i = 1; i < flat.length; i++) {
                Object elem = flat[i];
                boolean hasEllipsis = i + 1 < flat.length
                        && flat[i + 1] instanceof String ds && "...".equals(ds);
                if (hasEllipsis) {
                    String eVar = findEllipsisVar(elem, ellipsisBindings);
                    if (eVar != null) {
                        for (Object val : ellipsisBindings.get(eVar)) {
                            Map<String, Object> lb = new HashMap<>(bindings);
                            lb.put(eVar, val);
                            result.add(expandTemplate(elem, lb, ellipsisBindings, patternVars, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(elem, bindings, ellipsisBindings, patternVars, renames));
                }
            }
            // Splice the rest binding
            if (restTmpl != null) {
                Object restExpanded = expandTemplate(restTmpl, bindings, ellipsisBindings, patternVars, renames);
                // Splice Scheme list (Pair chain) into result
                Object cur = restExpanded;
                while (cur instanceof Pair p) {
                    result.add(p.car);
                    cur = p.cdr;
                }
                // If rest is a proper list ending in NIL, we're done.
                // If it's a symbol/other, it might be an empty list.
            }
            return result;
        }
        if (tmpl instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                boolean hasEllipsis = i + 1 < list.size()
                        && list.get(i + 1) instanceof String ds && "...".equals(ds);
                if (hasEllipsis) {
                    String eVar = findEllipsisVar(list.get(i), ellipsisBindings);
                    if (eVar != null) {
                        for (Object val : ellipsisBindings.get(eVar)) {
                            Map<String, Object> lb = new HashMap<>(bindings);
                            lb.put(eVar, val);
                            result.add(expandTemplate(list.get(i), lb, ellipsisBindings, patternVars, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(list.get(i), bindings, ellipsisBindings, patternVars, renames));
                }
            }
            return result;
        }
        return tmpl;
    }

    private String findEllipsisVar(Object tmpl, Map<String, List<Object>> ellipsisBindings) {
        if (tmpl instanceof String s && ellipsisBindings.containsKey(s)) return s;
        if (tmpl instanceof List<?> list) {
            for (Object e : list) {
                String found = findEllipsisVar(e, ellipsisBindings);
                if (found != null) return found;
            }
        }
        if (tmpl instanceof Pair) {
            Object cur = tmpl;
            while (cur instanceof Pair p) {
                String found = findEllipsisVar(p.car, ellipsisBindings);
                if (found != null) return found;
                cur = p.cdr;
            }
            if (cur != null && cur != SchemeValue.NIL) {
                return findEllipsisVar(cur, ellipsisBindings);
            }
        }
        return null;
    }
}
