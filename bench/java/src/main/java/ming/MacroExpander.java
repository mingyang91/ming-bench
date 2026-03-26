package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class MacroExpander {

    private final Evaluator evaluator;

    MacroExpander(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    private static Object unwrap(Object expr) {
        if (expr instanceof Evaluator.Located loc) return loc.datum;
        return expr;
    }

    private static boolean isKeyword(String sym) {
        return switch (sym) {
            case "if", "define", "lambda", "quote", "set!", "begin", "cond",
                 "let", "let*", "and", "or", "define-syntax", "syntax-rules",
                 "define-record-type", "case-lambda",
                 "letrec", "letrec*", "case", "do", "dynamic-wind",
                 "guard", "syntax-case", "syntax", "with-syntax" -> true;
            default -> false;
        };
    }

    @SuppressWarnings("unchecked")
    Evaluator.SyntaxRules evalSyntaxRules(Object expr, Evaluator.Env env) throws EvalError {
        List<?> form = (List<?>) unwrap(expr);
        if (form.isEmpty() || !"syntax-rules".equals(unwrap(form.get(0))))
            throw evaluator.posError("expected syntax-rules");
        List<?> literalsRaw = (List<?>) unwrap(form.get(1));
        List<String> literals = new ArrayList<>();
        for (Object lit : literalsRaw) literals.add((String) unwrap(lit));
        List<Object[]> rules = new ArrayList<>();
        for (int i = 2; i < form.size(); i++) {
            List<?> rule = (List<?>) unwrap(form.get(i));
            rules.add(new Object[]{rule.get(0), rule.get(1)});
        }
        return new Evaluator.SyntaxRules(literals, rules, env);
    }

    // Linearize a pattern (List or Pair chain) into elements + optional rest variable
    private static class LinearPattern {
        List<Object> elems = new ArrayList<>();
        Object rest = null; // non-null if dotted (e.g., (a b . rest))
    }

    private LinearPattern linearize(Object obj) {
        LinearPattern lp = new LinearPattern();
        Object raw = unwrap(obj);
        if (raw instanceof List<?> list) {
            for (Object e : list) lp.elems.add(e);
        } else if (raw instanceof Evaluator.Pair) {
            Object curr = raw;
            while (curr instanceof Evaluator.Pair pair) {
                lp.elems.add(pair.car);
                Object next = unwrap(pair.cdr);
                if (next instanceof Evaluator.Pair) {
                    curr = next;
                } else {
                    // dotted tail
                    lp.rest = next;
                    curr = null;
                }
            }
        }
        return lp;
    }

    @SuppressWarnings("unchecked")
    Object expandMacro(Evaluator.SyntaxRules sr, List<?> form) throws EvalError {
        Set<String> literalSet = new HashSet<>(sr.literals);
        for (Object[] rule : sr.rules) {
            Object patternObj = rule[0];
            Object template = rule[1];
            Set<String> ellipsisVars = new HashSet<>();
            Map<String, Object> bindings = matchPattern(patternObj, form, literalSet, ellipsisVars);
            if (bindings != null) {
                Set<String> patternVars = new HashSet<>(bindings.keySet());
                Map<String, String> gensymMap = new HashMap<>();
                return expandTemplate(template, bindings, patternVars, ellipsisVars, sr, gensymMap);
            }
        }
        throw evaluator.posError("no matching syntax-rules pattern");
    }

    private Map<String, Object> matchPattern(Object patternObj, List<?> form,
                                              Set<String> literals, Set<String> ellipsisVars) {
        LinearPattern lp = linearize(patternObj);
        List<Object> pattern = lp.elems;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, fi = 1;
        int patLen = pattern.size(), formLen = form.size();
        while (pi < patLen) {
            Object rawPat = unwrap(pattern.get(pi));
            boolean hasEllipsis = (pi + 1 < patLen && "...".equals(unwrap(pattern.get(pi + 1))));
            if (hasEllipsis) {
                if (!(rawPat instanceof String varName)) return null;
                int remaining = 0;
                for (int k = pi + 2; k < patLen; k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int endFi = formLen - remaining;
                List<Object> matched = new ArrayList<>();
                while (fi < endFi) { matched.add(form.get(fi)); fi++; }
                ellipsisVars.add(varName);
                bindings.put(varName, matched);
                pi += 2;
            } else if (rawPat instanceof String sym) {
                if (literals.contains(sym)) {
                    if (fi >= formLen) return null;
                    if (!sym.equals(unwrap(form.get(fi)))) return null;
                    fi++;
                } else {
                    if (fi >= formLen) return null;
                    bindings.put(sym, form.get(fi));
                    fi++;
                }
                pi++;
            } else if (rawPat instanceof List<?> || rawPat instanceof Evaluator.Pair) {
                // Sub-pattern matching (nested list/pair patterns)
                if (fi >= formLen) return null;
                Object formElem = form.get(fi);
                Object formRaw = unwrap(formElem);
                if (formRaw instanceof List<?> subForm) {
                    Map<String, Object> subBindings = matchSubPattern(rawPat, subForm, literals, ellipsisVars);
                    if (subBindings == null) return null;
                    bindings.putAll(subBindings);
                } else {
                    return null;
                }
                fi++; pi++;
            } else {
                if (fi >= formLen) return null;
                fi++; pi++;
            }
        }
        // Handle dotted tail (rest pattern variable)
        if (lp.rest != null) {
            Object restRaw = unwrap(lp.rest);
            if (restRaw instanceof String restVar) {
                List<Object> restElems = new ArrayList<>();
                while (fi < formLen) { restElems.add(form.get(fi)); fi++; }
                bindings.put(restVar, new Evaluator.Located(restElems, 0, 0));
            }
            return bindings;
        }
        return (fi == formLen) ? bindings : null;
    }

    private Map<String, Object> matchSubPattern(Object patternObj, List<?> form,
                                                  Set<String> literals, Set<String> ellipsisVars) {
        // Reuse matchPattern but starting from index 0 instead of 1
        LinearPattern lp = linearize(patternObj);
        List<Object> pattern = lp.elems;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 0, fi = 0;
        int patLen = pattern.size(), formLen = form.size();
        while (pi < patLen) {
            Object rawPat = unwrap(pattern.get(pi));
            boolean hasEllipsis = (pi + 1 < patLen && "...".equals(unwrap(pattern.get(pi + 1))));
            if (hasEllipsis) {
                if (!(rawPat instanceof String varName)) return null;
                int remaining = 0;
                for (int k = pi + 2; k < patLen; k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int endFi = formLen - remaining;
                List<Object> matched = new ArrayList<>();
                while (fi < endFi) { matched.add(form.get(fi)); fi++; }
                ellipsisVars.add(varName);
                bindings.put(varName, matched);
                pi += 2;
            } else if (rawPat instanceof String sym) {
                if (literals.contains(sym)) {
                    if (fi >= formLen) return null;
                    if (!sym.equals(unwrap(form.get(fi)))) return null;
                    fi++;
                } else {
                    if (fi >= formLen) return null;
                    bindings.put(sym, form.get(fi));
                    fi++;
                }
                pi++;
            } else {
                if (fi >= formLen) return null;
                fi++; pi++;
            }
        }
        if (lp.rest != null) {
            Object restRaw = unwrap(lp.rest);
            if (restRaw instanceof String restVar) {
                List<Object> restElems = new ArrayList<>();
                while (fi < formLen) { restElems.add(form.get(fi)); fi++; }
                bindings.put(restVar, new Evaluator.Located(restElems, 0, 0));
            }
            return bindings;
        }
        return (fi == formLen) ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Set<String> patternVars, Set<String> ellipsisVars,
                                   Evaluator.SyntaxRules sr, Map<String, String> gensymMap) throws EvalError {
        Object raw = unwrap(template);
        if (raw instanceof String sym) {
            if (patternVars.contains(sym) && !ellipsisVars.contains(sym)) return bindings.get(sym);
            if (ellipsisVars.contains(sym)) return bindings.get(sym);
            if (isKeyword(sym) || sym.equals("...")) return sym;
            try {
                sr.defEnv.lookup(sym);
                return new Evaluator.ResolvedRef(sym, sr.defEnv);
            } catch (EvalError e) {
                return gensymMap.computeIfAbsent(sym, k -> evaluator.gensym(k));
            }
        }
        if (raw instanceof Boolean || raw instanceof Long ||
            raw instanceof Evaluator.SchemeString || raw instanceof Evaluator.SchemeChar) return raw;
        if (raw instanceof Evaluator.Pair) {
            return expandPairTemplate(raw, bindings, patternVars, ellipsisVars, sr, gensymMap);
        }
        if (raw instanceof List<?> tmplList) {
            // Don't expand inside quote forms
            if (!tmplList.isEmpty() && "quote".equals(unwrap(tmplList.get(0)))) {
                return template;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                boolean hasEllipsis = (i + 1 < tmplList.size() && "...".equals(unwrap(tmplList.get(i + 1))));
                if (hasEllipsis) {
                    Set<String> usedEllipsis = findEllipsisVarsInTemplate(tmplList.get(i), ellipsisVars);
                    if (!usedEllipsis.isEmpty()) {
                        String anyVar = usedEllipsis.iterator().next();
                        List<Object> varList = (List<Object>) bindings.get(anyVar);
                        for (int j = 0; j < varList.size(); j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String ev : usedEllipsis) {
                                List<Object> evList = (List<Object>) bindings.get(ev);
                                iterBindings.put(ev, evList.get(j));
                            }
                            Set<String> adjustedEllipsis = new HashSet<>(ellipsisVars);
                            adjustedEllipsis.removeAll(usedEllipsis);
                            result.add(expandTemplate(tmplList.get(i), iterBindings, patternVars,
                                                       adjustedEllipsis, sr, gensymMap));
                        }
                    }
                    i++;
                } else {
                    result.add(expandTemplate(tmplList.get(i), bindings, patternVars,
                                               ellipsisVars, sr, gensymMap));
                }
            }
            return new Evaluator.Located(result, 0, 0);
        }
        return raw;
    }

    @SuppressWarnings("unchecked")
    private Object expandPairTemplate(Object pairChain, Map<String, Object> bindings,
                                       Set<String> patternVars, Set<String> ellipsisVars,
                                       Evaluator.SyntaxRules sr, Map<String, String> gensymMap) throws EvalError {
        // Linearize the Pair chain into elements + rest
        List<Object> elems = new ArrayList<>();
        Object rest = null;
        Object curr = pairChain;
        while (curr instanceof Evaluator.Pair pair) {
            elems.add(pair.car);
            Object next = unwrap(pair.cdr);
            if (next instanceof Evaluator.Pair) {
                curr = next;
            } else {
                rest = next;
                curr = null;
            }
        }
        // Expand each element
        List<Object> result = new ArrayList<>();
        for (int i = 0; i < elems.size(); i++) {
            boolean hasEllipsis = (i + 1 < elems.size() && "...".equals(unwrap(elems.get(i + 1))));
            if (hasEllipsis) {
                Set<String> usedEllipsis = findEllipsisVarsInTemplate(elems.get(i), ellipsisVars);
                if (!usedEllipsis.isEmpty()) {
                    String anyVar = usedEllipsis.iterator().next();
                    List<Object> varList = (List<Object>) bindings.get(anyVar);
                    for (int j = 0; j < varList.size(); j++) {
                        Map<String, Object> iterBindings = new HashMap<>(bindings);
                        for (String ev : usedEllipsis) {
                            List<Object> evList = (List<Object>) bindings.get(ev);
                            iterBindings.put(ev, evList.get(j));
                        }
                        Set<String> adjustedEllipsis = new HashSet<>(ellipsisVars);
                        adjustedEllipsis.removeAll(usedEllipsis);
                        result.add(expandTemplate(elems.get(i), iterBindings, patternVars,
                                                   adjustedEllipsis, sr, gensymMap));
                    }
                }
                i++;
            } else {
                result.add(expandTemplate(elems.get(i), bindings, patternVars,
                                           ellipsisVars, sr, gensymMap));
            }
        }
        // Handle the rest/tail: expand it and splice into the result list
        if (rest != null) {
            Object expandedRest = expandTemplate(rest, bindings, patternVars, ellipsisVars, sr, gensymMap);
            Object unwrappedRest = unwrap(expandedRest);
            if (unwrappedRest instanceof List<?> restList) {
                result.addAll(restList);
            }
        }
        return new Evaluator.Located(result, 0, 0);
    }

    Map<String, Object> matchSyntaxCasePattern(Object patternRaw, Object datum,
                                                  Set<String> literals, Set<String> ellipsisVars) {
        List<?> pattern = (List<?>) unwrap(patternRaw);
        List<?> form;
        if (datum instanceof List<?> l) form = l;
        else return null;
        return matchPattern(pattern, form, literals, ellipsisVars);
    }

    Object expandSyntaxTemplate(Object template, Map<String, Object> bindings,
                                 Set<String> patternVars, Set<String> ellipsisVars,
                                 Evaluator.Env defEnv, Set<String> defTimeNames) throws EvalError {
        Evaluator.SyntaxRules fakeSr = new Evaluator.SyntaxRules(List.of(), List.of(), defEnv);
        Map<String, String> gensymMap = new HashMap<>();
        return expandTemplateSC(template, bindings, patternVars, ellipsisVars, fakeSr, gensymMap, defTimeNames);
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplateSC(Object template, Map<String, Object> bindings,
                                     Set<String> patternVars, Set<String> ellipsisVars,
                                     Evaluator.SyntaxRules sr, Map<String, String> gensymMap,
                                     Set<String> defTimeNames) throws EvalError {
        Object raw = unwrap(template);
        if (raw instanceof String sym) {
            if (patternVars.contains(sym) && !ellipsisVars.contains(sym)) return bindings.get(sym);
            if (ellipsisVars.contains(sym)) return bindings.get(sym);
            if (isKeyword(sym) || sym.equals("...")) return sym;
            // Resolve names that existed at definition time (proper hygiene)
            if (defTimeNames != null && defTimeNames.contains(sym)) {
                try {
                    sr.defEnv.lookup(sym);
                    return new Evaluator.ResolvedRef(sym, sr.defEnv);
                } catch (EvalError e) {
                    // fallthrough
                }
            }
            // Leave other names as plain strings - they'll resolve at call site
            return sym;
        }
        if (raw instanceof Boolean || raw instanceof Long ||
            raw instanceof Evaluator.SchemeString || raw instanceof Evaluator.SchemeChar) return raw;
        if (raw instanceof List<?> tmplList) {
            if (!tmplList.isEmpty() && "quote".equals(unwrap(tmplList.get(0)))) {
                return template;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                boolean hasEllipsis = (i + 1 < tmplList.size() && "...".equals(unwrap(tmplList.get(i + 1))));
                if (hasEllipsis) {
                    Set<String> usedEllipsis = findEllipsisVarsInTemplate(tmplList.get(i), ellipsisVars);
                    if (!usedEllipsis.isEmpty()) {
                        String anyVar = usedEllipsis.iterator().next();
                        List<Object> varList = (List<Object>) bindings.get(anyVar);
                        for (int j = 0; j < varList.size(); j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String ev : usedEllipsis) {
                                List<Object> evList = (List<Object>) bindings.get(ev);
                                iterBindings.put(ev, evList.get(j));
                            }
                            Set<String> adjustedEllipsis = new HashSet<>(ellipsisVars);
                            adjustedEllipsis.removeAll(usedEllipsis);
                            result.add(expandTemplateSC(tmplList.get(i), iterBindings, patternVars,
                                                         adjustedEllipsis, sr, gensymMap, defTimeNames));
                        }
                    }
                    i++;
                } else {
                    result.add(expandTemplateSC(tmplList.get(i), bindings, patternVars,
                                                 ellipsisVars, sr, gensymMap, defTimeNames));
                }
            }
            return new Evaluator.Located(result, 0, 0);
        }
        return raw;
    }

    private Set<String> findEllipsisVarsInTemplate(Object template, Set<String> ellipsisVars) {
        Object raw = unwrap(template);
        Set<String> found = new HashSet<>();
        if (raw instanceof String sym && ellipsisVars.contains(sym)) {
            found.add(sym);
        } else if (raw instanceof List<?> list) {
            for (Object elem : list) found.addAll(findEllipsisVarsInTemplate(elem, ellipsisVars));
        } else if (raw instanceof Evaluator.Pair) {
            Object curr = raw;
            while (curr instanceof Evaluator.Pair pair) {
                found.addAll(findEllipsisVarsInTemplate(pair.car, ellipsisVars));
                Object next = unwrap(pair.cdr);
                if (next instanceof Evaluator.Pair) {
                    curr = next;
                } else {
                    found.addAll(findEllipsisVarsInTemplate(pair.cdr, ellipsisVars));
                    break;
                }
            }
        }
        return found;
    }
}
