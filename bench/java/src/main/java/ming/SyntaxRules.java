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
        "let", "cond", "define-syntax", "syntax-rules"
    );

    SyntaxRules(List<String> literals, List<Rule> rules, Env defEnv) {
        this.literals = literals;
        this.rules = rules;
        this.defEnv = defEnv;
    }

    static class Rule {
        final List<?> pattern;
        final Object template;
        Rule(List<?> pattern, Object template) {
            this.pattern = pattern;
            this.template = template;
        }
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
            if (!(clause.get(0) instanceof List<?> pattern))
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

                Env expandEnv = new Env(callEnv);
                for (var entry : renames.entrySet()) {
                    try {
                        Object val = defEnv.lookup(entry.getKey());
                        expandEnv.define(entry.getValue(), val);
                    } catch (EvalError e) { /* not bound at def site, skip */ }
                }

                return new Object[] { expanded, expandEnv };
            }
        }
        throw new EvalError("syntax-rules: no matching pattern for " + SchemeValue.toStr(form));
    }

    private boolean matchPattern(List<?> pattern, List<?> form,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings) {
        int pi = 1, fi = 1; // skip macro name in both pattern and form
        while (pi < pattern.size()) {
            boolean hasEllipsis = pi + 1 < pattern.size() && "...".equals(pattern.get(pi + 1));
            if (hasEllipsis) {
                if (!(pattern.get(pi) instanceof String varName)) return false;
                int remaining = 0;
                for (int j = pi + 2; j < pattern.size(); j++) {
                    if (!"...".equals(pattern.get(j))) remaining++;
                }
                int available = form.size() - fi - remaining;
                if (available < 0) return false;
                List<Object> collected = new ArrayList<>();
                for (int i = 0; i < available; i++) collected.add(form.get(fi + i));
                ellipsisBindings.put(varName, collected);
                fi += available;
                pi += 2;
            } else {
                if (fi >= form.size()) return false;
                Object pe = pattern.get(pi);
                if (pe instanceof String s && !s.startsWith("\"")) {
                    if (literals.contains(s)) {
                        if (!s.equals(form.get(fi))) return false;
                    } else {
                        bindings.put(s, form.get(fi));
                    }
                } else {
                    if (!pe.equals(form.get(fi))) return false;
                }
                pi++;
                fi++;
            }
        }
        return fi == form.size();
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
        return null;
    }
}
