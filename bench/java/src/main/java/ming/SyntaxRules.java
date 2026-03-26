package ming;

import java.util.*;

public class SyntaxRules {
    final List<String> literals;
    final List<Object[]> rules; // each: {pattern (List), template (Object)}
    final Environment defEnv;
    private static int gensymCounter = 0;

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "set!", "lambda", "case-lambda", "begin",
        "let", "let*", "letrec", "letrec*", "case", "do",
        "cond", "and", "or", "define-syntax", "syntax-rules",
        "define-record-type", "call/cc", "call-with-current-continuation",
        "dynamic-wind", "guard", "syntax-case", "syntax", "with-syntax"
    );

    public SyntaxRules(List<String> literals, List<Object[]> rules, Environment defEnv) {
        this.literals = literals;
        this.rules = rules;
        this.defEnv = defEnv;
    }

    private static String gensym(String base) {
        return base + "__m" + (gensymCounter++);
    }

    @SuppressWarnings("unchecked")
    public Object expand(List<?> form, Environment useEnv) throws EvalError {
        for (Object[] rule : rules) {
            List<?> pattern = (List<?>) rule[0];
            Object template = rule[1];

            Map<String, Object> bindings = new HashMap<>();
            Map<String, List<Object>> ellipsisBindings = new HashMap<>();

            if (matchPattern(pattern, form, bindings, ellipsisBindings)) {
                Set<String> patVars = new HashSet<>(bindings.keySet());
                patVars.addAll(ellipsisBindings.keySet());
                Map<String, String> renames = new HashMap<>();
                collectNonPatternSymbols(template, patVars, renames);

                // Inject definition-site bindings for renamed symbols into defEnv
                // (not useEnv) so they persist across cached macro expansions
                for (Map.Entry<String, String> entry : renames.entrySet()) {
                    try {
                        Object val = defEnv.lookup(entry.getKey());
                        defEnv.define(entry.getValue(), val);
                    } catch (EvalError ignored) {
                        // Introduced variable with no definition-site binding —
                        // define in useEnv so it resolves at use site
                        useEnv.define(entry.getValue(), entry.getValue());
                    }
                }

                return expandTemplate(template, bindings, ellipsisBindings, renames);
            }
        }
        throw new EvalError("no matching pattern for macro");
    }

    // Pattern matching — skip index 0 (macro name) in both pattern and form
    private boolean matchPattern(List<?> pattern, List<?> form,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings) {
        return matchElements(pattern, form, bindings, ellipsisBindings, 1, 1);
    }

    private boolean matchElements(List<?> pattern, List<?> form,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings,
            int pi, int fi) {
        while (pi < pattern.size()) {
            boolean hasEllipsis = pi + 1 < pattern.size() && isEllipsis(pattern.get(pi + 1));

            if (hasEllipsis) {
                Object patElem = pattern.get(pi);
                // Count remaining non-ellipsis pattern elements after this ellipsis pair
                int remainingAfter = 0;
                for (int k = pi + 2; k < pattern.size(); k++) {
                    if (!isEllipsis(pattern.get(k))) remainingAfter++;
                }
                int endFi = form.size() - remainingAfter;

                if (patElem instanceof SchemeSymbol sym && !literals.contains(sym.name())) {
                    List<Object> collected = new ArrayList<>();
                    while (fi < endFi) {
                        collected.add(form.get(fi));
                        fi++;
                    }
                    ellipsisBindings.put(sym.name(), collected);
                }
                pi += 2; // skip element + ellipsis
            } else {
                if (fi >= form.size()) return false;
                Object patElem = pattern.get(pi);
                Object formElem = form.get(fi);

                if (patElem instanceof SchemeSymbol sym) {
                    if (literals.contains(sym.name())) {
                        if (!(formElem instanceof SchemeSymbol fs) || !fs.name().equals(sym.name()))
                            return false;
                    } else {
                        bindings.put(sym.name(), formElem);
                    }
                } else if (patElem instanceof List<?> subPat && formElem instanceof List<?> subForm) {
                    if (!matchElements(subPat, subForm, bindings, ellipsisBindings, 0, 0))
                        return false;
                } else {
                    if (!patElem.equals(formElem)) return false;
                }
                pi++;
                fi++;
            }
        }
        return fi == form.size();
    }

    private boolean isEllipsis(Object o) {
        return o instanceof SchemeSymbol s && s.name().equals("...");
    }

    private void collectNonPatternSymbols(Object template, Set<String> patVars,
            Map<String, String> renames) {
        if (template instanceof SchemeSymbol sym) {
            String name = sym.name();
            if (!patVars.contains(name) && !SPECIAL_FORMS.contains(name)
                    && !name.equals("...") && !literals.contains(name)
                    && !renames.containsKey(name)
                    && hasDefEnvBinding(name)) {
                renames.put(name, gensym(name));
            }
        } else if (template instanceof List<?> list) {
            for (Object elem : list) {
                collectNonPatternSymbols(elem, patVars, renames);
            }
        }
    }

    private boolean hasDefEnvBinding(String name) {
        try {
            defEnv.lookup(name);
            return true;
        } catch (EvalError e) {
            return false;
        }
    }

    private Object expandTemplate(Object template, Map<String, Object> bindings,
            Map<String, List<Object>> ellipsisBindings, Map<String, String> renames) {
        if (template instanceof SchemeSymbol sym) {
            String name = sym.name();
            if (bindings.containsKey(name)) return bindings.get(name);
            if (renames.containsKey(name)) return new SchemeSymbol(renames.get(name));
            return sym;
        }

        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && isEllipsis(list.get(i + 1))) {
                    Object elemTemplate = list.get(i);
                    Set<String> usedEllipsis = new HashSet<>();
                    findEllipsisVars(elemTemplate, ellipsisBindings, usedEllipsis);

                    if (!usedEllipsis.isEmpty()) {
                        String firstVar = usedEllipsis.iterator().next();
                        int count = ellipsisBindings.get(firstVar).size();
                        for (int j = 0; j < count; j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String var : usedEllipsis) {
                                iterBindings.put(var, ellipsisBindings.get(var).get(j));
                            }
                            result.add(expandTemplate(elemTemplate, iterBindings, ellipsisBindings, renames));
                        }
                    }
                    i++; // skip ellipsis
                } else {
                    result.add(expandTemplate(list.get(i), bindings, ellipsisBindings, renames));
                }
            }
            return result;
        }

        return template;
    }

    private void findEllipsisVars(Object template, Map<String, List<Object>> ellipsisBindings,
            Set<String> result) {
        if (template instanceof SchemeSymbol sym) {
            if (ellipsisBindings.containsKey(sym.name())) result.add(sym.name());
        } else if (template instanceof List<?> list) {
            for (Object elem : list) findEllipsisVars(elem, ellipsisBindings, result);
        }
    }
}
