package ming;

import java.util.*;

public class MacroExpander {
    private static int gensymCounter = 0;

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "let", "let*", "letrec", "begin", "set!", "define", "lambda",
        "quote", "cond", "and", "or", "call/cc", "call-with-current-continuation",
        "define-syntax", "syntax-rules", "quasiquote", "unquote", "unquote-splicing"
    );

    public static SchemeValue expand(SchemeValue.SyntaxRulesVal macro, SchemeValue.ListVal form) throws EvalError {
        for (int i = 0; i < macro.patterns().size(); i++) {
            var bindings = new Bindings();
            if (matchPattern(macro.patterns().get(i), form, macro.literals(), bindings)) {
                int mark = ++gensymCounter;
                var renames = new HashMap<String, String>();
                return expandTemplate(macro.templates().get(i), bindings, macro.defEnv(), mark, renames);
            }
        }
        throw new EvalError("syntax error: no matching pattern for macro");
    }

    static class Bindings {
        final Map<String, SchemeValue> regular = new HashMap<>();
        final Map<String, List<SchemeValue>> ellipsis = new HashMap<>();
    }

    private static boolean matchPattern(SchemeValue pattern, SchemeValue form,
                                        List<String> literals, Bindings bindings) {
        if (pattern instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (name.equals("_")) return true;
            if (literals.contains(name)) {
                return form instanceof SchemeValue.SymbolVal fs && fs.name().equals(name);
            }
            bindings.regular.put(name, form);
            return true;
        }
        if (pattern instanceof SchemeValue.ListVal patList) {
            if (!(form instanceof SchemeValue.ListVal formList)) return false;
            var pe = patList.elements();
            var fe = formList.elements();

            int ellipsisIdx = -1;
            for (int i = 0; i < pe.size(); i++) {
                if (pe.get(i) instanceof SchemeValue.SymbolVal s && s.name().equals("...")) {
                    ellipsisIdx = i;
                    break;
                }
            }

            if (ellipsisIdx == -1) {
                if (pe.size() != fe.size()) return false;
                for (int i = 0; i < pe.size(); i++) {
                    if (!matchPattern(pe.get(i), fe.get(i), literals, bindings)) return false;
                }
                return true;
            }

            int before = ellipsisIdx - 1;
            int after = pe.size() - ellipsisIdx - 1;
            if (fe.size() < before + after) return false;

            for (int i = 0; i < before; i++) {
                if (!matchPattern(pe.get(i), fe.get(i), literals, bindings)) return false;
            }

            SchemeValue ellipsisPat = pe.get(before);
            int count = fe.size() - before - after;
            var matched = new ArrayList<SchemeValue>();
            for (int i = 0; i < count; i++) {
                matched.add(fe.get(before + i));
            }
            if (ellipsisPat instanceof SchemeValue.SymbolVal sv && !literals.contains(sv.name())) {
                bindings.ellipsis.put(sv.name(), matched);
            }

            for (int i = 0; i < after; i++) {
                if (!matchPattern(pe.get(ellipsisIdx + 1 + i), fe.get(fe.size() - after + i), literals, bindings))
                    return false;
            }
            return true;
        }
        if (pattern instanceof SchemeValue.BoolVal b) {
            return form instanceof SchemeValue.BoolVal fb && fb.value() == b.value();
        }
        if (pattern instanceof SchemeValue.IntVal n) {
            return form instanceof SchemeValue.IntVal fn && fn.value() == n.value();
        }
        return false;
    }

    private static SchemeValue expandTemplate(SchemeValue template, Bindings bindings,
                                               Environment defEnv, int mark,
                                               Map<String, String> renames) throws EvalError {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (bindings.regular.containsKey(name)) {
                return bindings.regular.get(name);
            }
            if (bindings.ellipsis.containsKey(name)) {
                throw new EvalError("macro: ellipsis variable outside ellipsis context");
            }
            if (SPECIAL_FORMS.contains(name) || name.equals("...")) {
                return template;
            }
            // Hygiene: try definition environment
            try {
                return defEnv.get(name);
            } catch (EvalError e) {
                // Introduced symbol — gensym it
                String gensym = renames.computeIfAbsent(name, k -> "__m" + mark + "_" + k);
                return new SchemeValue.SymbolVal(gensym);
            }
        }

        if (template instanceof SchemeValue.ListVal listVal) {
            var elems = listVal.elements();
            var result = new ArrayList<SchemeValue>();

            for (int i = 0; i < elems.size(); i++) {
                boolean hasEllipsis = i + 1 < elems.size()
                    && elems.get(i + 1) instanceof SchemeValue.SymbolVal s
                    && s.name().equals("...");

                if (hasEllipsis) {
                    SchemeValue elemTemplate = elems.get(i);
                    Set<String> usedVars = findEllipsisVars(elemTemplate, bindings);
                    if (!usedVars.isEmpty()) {
                        String firstVar = usedVars.iterator().next();
                        List<SchemeValue> values = bindings.ellipsis.get(firstVar);
                        for (int j = 0; j < values.size(); j++) {
                            Bindings iter = new Bindings();
                            iter.regular.putAll(bindings.regular);
                            iter.ellipsis.putAll(bindings.ellipsis);
                            for (String v : usedVars) {
                                List<SchemeValue> vv = bindings.ellipsis.get(v);
                                if (j < vv.size()) iter.regular.put(v, vv.get(j));
                            }
                            result.add(expandTemplate(elemTemplate, iter, defEnv, mark, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(elems.get(i), bindings, defEnv, mark, renames));
                }
            }
            return new SchemeValue.ListVal(result);
        }

        return template;
    }

    private static Set<String> findEllipsisVars(SchemeValue template, Bindings bindings) {
        var result = new HashSet<String>();
        collectEllipsisVars(template, bindings, result);
        return result;
    }

    private static void collectEllipsisVars(SchemeValue template, Bindings bindings, Set<String> result) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            if (bindings.ellipsis.containsKey(sym.name())) {
                result.add(sym.name());
            }
        } else if (template instanceof SchemeValue.ListVal list) {
            for (var elem : list.elements()) {
                collectEllipsisVars(elem, bindings, result);
            }
        }
    }
}
