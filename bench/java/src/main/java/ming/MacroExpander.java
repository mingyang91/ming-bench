package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import ming.Evaluator.Builtin;
import ming.Evaluator.Env;

/**
 * Handles syntax-rules macro expansion, define-syntax, and define-record-type.
 */
final class MacroExpander {

    private final Evaluator evaluator;

    MacroExpander(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    private Object unwrap(Object expr) {
        if (expr instanceof Token t) return t.value();
        return expr;
    }

    private String posStr(Pos p) {
        return p != null ? " at " + p : "";
    }

    @SuppressWarnings("unchecked")
    Object[] expandMacroForm(SyntaxRules macro, List<?> form, Env env, Pos pos) throws EvalError {
        for (int i = 0; i < macro.patterns.size(); i++) {
            List<Object> pattern = macro.patterns.get(i);
            Map<String, Object> bindings = new HashMap<>();
            Set<String> patVars = new java.util.HashSet<>();
            Set<String> ellipsisVars = new java.util.HashSet<>();
            collectPatternVars(pattern, 1, macro.literals, patVars, ellipsisVars);
            if (matchElements(pattern, 1, form, 1, macro.literals, patVars, bindings)) {
                Map<String, String> renames = new HashMap<>();
                Object expanded = expandTemplate(macro.templates.get(i), bindings, ellipsisVars, patVars, renames);
                for (Map.Entry<String, String> entry : renames.entrySet()) {
                    try {
                        env.define(entry.getValue(), macro.defEnv.lookup(entry.getKey()));
                    } catch (EvalError ignore) {}
                }
                return new Object[]{expanded, env};
            }
        }
        throw new EvalError("no matching syntax-rules pattern" + posStr(pos));
    }

    @SuppressWarnings("unchecked")
    void evalDefineSyntax(List<?> list, Env env, Pos pos) throws EvalError {
        if (list.size() != 3) throw new EvalError("define-syntax requires 2 arguments" + posStr(pos));
        Object nameRaw = unwrap(list.get(1));
        if (!(nameRaw instanceof String macroName))
            throw new EvalError("define-syntax: name must be a symbol" + posStr(pos));
        Object rulesExpr = unwrap(list.get(2));
        if (!(rulesExpr instanceof List<?> rulesList) || rulesList.size() < 2
                || !"syntax-rules".equals(unwrap(rulesList.get(0))))
            throw new EvalError("define-syntax: expected syntax-rules" + posStr(pos));
        Object literalsExpr = unwrap(rulesList.get(1));
        List<String> literals = new ArrayList<>();
        if (literalsExpr instanceof List<?> litList) {
            for (Object lit : litList) {
                if (unwrap(lit) instanceof String s) literals.add(s);
            }
        }
        List<List<Object>> patterns = new ArrayList<>();
        List<Object> templates = new ArrayList<>();
        for (int i = 2; i < rulesList.size(); i++) {
            Object rule = unwrap(rulesList.get(i));
            if (!(rule instanceof List<?> rulePair) || rulePair.size() != 2)
                throw new EvalError("syntax-rules: each rule must be (pattern template)" + posStr(pos));
            Object pat = unwrap(rulePair.get(0));
            if (!(pat instanceof List<?> patList))
                throw new EvalError("syntax-rules: pattern must be a list" + posStr(pos));
            patterns.add((List<Object>) patList);
            templates.add(rulePair.get(1));
        }
        env.define(macroName, new SyntaxRules(literals, patterns, templates, env));
    }

    @SuppressWarnings("unchecked")
    void evalDefineRecordType(List<?> list, Env env, Pos pos) throws EvalError {
        if (list.size() < 4) throw new EvalError("define-record-type requires at least 3 arguments" + posStr(pos));
        String typeName = unwrap(list.get(1)) instanceof String s ? s : null;
        if (typeName == null) throw new EvalError("define-record-type: name must be a symbol" + posStr(pos));

        Object ctorSpec = unwrap(list.get(2));
        if (!(ctorSpec instanceof List<?> ctorList) || ctorList.size() < 1)
            throw new EvalError("define-record-type: invalid constructor spec" + posStr(pos));
        String ctorName = unwrap(ctorList.get(0)) instanceof String s ? s : null;
        if (ctorName == null) throw new EvalError("define-record-type: constructor name must be a symbol" + posStr(pos));
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorList.size(); i++) {
            String f = unwrap(ctorList.get(i)) instanceof String s ? s : null;
            if (f == null) throw new EvalError("define-record-type: field name must be a symbol" + posStr(pos));
            ctorFields.add(f);
        }

        String predName = unwrap(list.get(3)) instanceof String s ? s : null;
        if (predName == null) throw new EvalError("define-record-type: predicate must be a symbol" + posStr(pos));

        RecordType rt = new RecordType(typeName, ctorFields);

        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int i = 0; i < ctorFields.size(); i++) {
            fieldIndex.put(ctorFields.get(i), i);
        }

        env.define(ctorName, new Builtin(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + " requires " + ctorFields.size() + " arguments, got " + args.size());
            return new SchemeRecord(rt, args.toArray());
        }));

        env.define(predName, new Builtin(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + " requires 1 argument");
            return args.get(0) instanceof SchemeRecord r && r.type == rt;
        }));

        for (int i = 4; i < list.size(); i++) {
            Object fieldSpec = unwrap(list.get(i));
            if (!(fieldSpec instanceof List<?> fList) || fList.size() < 2)
                throw new EvalError("define-record-type: invalid field spec" + posStr(pos));
            String fieldName = unwrap(fList.get(0)) instanceof String s ? s : null;
            String accessorName = unwrap(fList.get(1)) instanceof String s2 ? s2 : null;
            if (fieldName == null || accessorName == null)
                throw new EvalError("define-record-type: field spec names must be symbols" + posStr(pos));
            Integer idx = fieldIndex.get(fieldName);
            if (idx == null)
                throw new EvalError("define-record-type: unknown field " + fieldName + posStr(pos));
            final int fi = idx;
            env.define(accessorName, new Builtin(accessorName, args -> {
                if (args.size() != 1) throw new EvalError(accessorName + " requires 1 argument");
                if (!(args.get(0) instanceof SchemeRecord r) || r.type != rt)
                    throw new EvalError(accessorName + ": not a " + typeName);
                return r.fields[fi];
            }));
        }
    }

    @SuppressWarnings("unchecked")
    void collectPatternVars(List<Object> pattern, int start, List<String> literals,
                             Set<String> patVars, Set<String> ellipsisVars) {
        for (int i = start; i < pattern.size(); i++) {
            Object raw = unwrap(pattern.get(i));
            if (raw instanceof String sym && !literals.contains(sym) && !"...".equals(sym)) {
                patVars.add(sym);
                if (i + 1 < pattern.size() && "...".equals(unwrap(pattern.get(i + 1)))) {
                    ellipsisVars.add(sym);
                }
            } else if (raw instanceof List<?> nested) {
                collectPatternVars((List<Object>) nested, 0, literals, patVars, ellipsisVars);
            }
        }
    }

    boolean matchElements(List<Object> pattern, int pi, List<?> input, int ii,
                           List<String> literals, Set<String> patVars,
                           Map<String, Object> bindings) {
        while (pi < pattern.size()) {
            Object patRaw = unwrap(pattern.get(pi));
            if (pi + 1 < pattern.size() && "...".equals(unwrap(pattern.get(pi + 1)))) {
                if (!(patRaw instanceof String var)) return false;
                int remaining = 0;
                for (int k = pi + 2; k < pattern.size(); k++) {
                    if (!"...".equals(unwrap(pattern.get(k)))) remaining++;
                }
                int available = input.size() - ii - remaining;
                if (available < 0) return false;
                List<Object> collected = new ArrayList<>();
                for (int j = 0; j < available; j++) {
                    collected.add(input.get(ii + j));
                }
                bindings.put(var, collected);
                ii += available;
                pi += 2;
            } else {
                if (ii >= input.size()) return false;
                if (patRaw instanceof String sym && patVars.contains(sym)) {
                    bindings.put(sym, input.get(ii));
                } else {
                    Object inRaw = unwrap(input.get(ii));
                    if (!Objects.equals(patRaw, inRaw)) return false;
                }
                pi++;
                ii++;
            }
        }
        return ii == input.size();
    }

    @SuppressWarnings("unchecked")
    Object expandTemplate(Object template, Map<String, Object> bindings,
                           Set<String> ellipsisVars, Set<String> patVars,
                           Map<String, String> renames) {
        Object raw = unwrap(template);
        if (raw instanceof String sym) {
            if (patVars.contains(sym)) {
                return bindings.get(sym);
            }
            if ("...".equals(sym)) return template;
            if (!Evaluator.SPECIAL_FORMS.contains(sym)) {
                return renames.computeIfAbsent(sym, k -> evaluator.gensym(k));
            }
            return sym;
        }
        if (raw instanceof List<?> list) {
            // Don't expand inside quotes (preserve literal symbols)
            if (!list.isEmpty() && "quote".equals(unwrap(list.get(0)))) {
                return raw;
            }
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && "...".equals(unwrap(list.get(i + 1)))) {
                    String var = findEllipsisVar(list.get(i), ellipsisVars);
                    if (var != null && bindings.get(var) instanceof List<?> vals) {
                        for (Object v : vals) {
                            Map<String, Object> singleBindings = new HashMap<>(bindings);
                            singleBindings.put(var, v);
                            result.add(expandTemplate(list.get(i), singleBindings,
                                    Set.of(), patVars, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(list.get(i), bindings, ellipsisVars, patVars, renames));
                }
            }
            SExpr sexpr = new SExpr(template instanceof SExpr s ? s.pos : null);
            sexpr.addAll(result);
            return sexpr;
        }
        return raw;
    }

    private String findEllipsisVar(Object template, Set<String> ellipsisVars) {
        Object raw = unwrap(template);
        if (raw instanceof String sym && ellipsisVars.contains(sym)) return sym;
        if (raw instanceof List<?> list) {
            for (Object elem : list) {
                String found = findEllipsisVar(elem, ellipsisVars);
                if (found != null) return found;
            }
        }
        return null;
    }
}
