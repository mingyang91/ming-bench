package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.ArrayDeque;

import static ming.SchemeReader.deepUnwrap;
import static ming.SchemeReader.unwrap;
import static ming.Evaluator.VOID;

final class SyntaxCaseHandler {

    @FunctionalInterface
    interface EvalFn {
        Object eval(Object expr, Environment env) throws EvalError, ContinuationException, SchemeRaiseException;
    }

    record MacroTransformer(Object transformer, Environment defEnv) {}

    private static class SyntaxCaseContext {
        final Map<String, Object> bindings;
        final Map<String, List<Object>> ellipsisBindings;
        final Set<String> patternVars;
        final Environment defEnv;
        SyntaxCaseContext(Map<String, Object> b, Map<String, List<Object>> eb, Set<String> pv, Environment de) {
            this.bindings = b; this.ellipsisBindings = eb; this.patternVars = pv; this.defEnv = de;
        }
    }

    final ArrayDeque<SyntaxCaseContext> syntaxCaseStack = new ArrayDeque<>();
    Environment currentMacroDefEnv = null;

    static final Set<String> SYNTAX_SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "set!", "lambda", "begin", "let", "let*", "letrec",
        "cond", "and", "or", "define-syntax", "syntax-rules", "syntax-case", "syntax",
        "with-syntax", "case-lambda", "letrec*", "do", "case", "define-record-type",
        "guard", "dynamic-wind", "call/cc", "call-with-current-continuation"
    );

    private static int syntaxGensymCounter = 0;
    static String syntaxGensym(String base) {
        return base + "__sc" + (syntaxGensymCounter++);
    }

    private final EvalFn evalFn;

    SyntaxCaseHandler(EvalFn evalFn) {
        this.evalFn = evalFn;
    }

    @SuppressWarnings("unchecked")
    Object evalSyntaxCase(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("syntax-case: bad syntax");
        Object stxVal = evalFn.eval(args.get(0), env);
        Object form = (stxVal instanceof SyntaxObject so) ? so.datum() : stxVal;
        List<Object> formList;
        if (form instanceof List<?> jl) {
            formList = (List<Object>) jl;
        } else if (form instanceof SchemePair || form instanceof SchemeNil) {
            formList = schemePairToJavaList(form);
        } else {
            formList = null;
        }

        List<?> litList = (List<?>) unwrap(args.get(1));
        List<String> literals = new ArrayList<>();
        for (Object lit : litList)
            literals.add(((SchemeSymbol) unwrap(lit)).name());

        for (int ci = 2; ci < args.size(); ci++) {
            List<?> clause = (List<?>) unwrap(args.get(ci));
            Object rawPattern = deepUnwrap(clause.get(0));

            Map<String, Object> bindings = new HashMap<>();
            Map<String, List<Object>> ellipsisBindings = new HashMap<>();

            boolean matched;
            if (rawPattern instanceof List<?> pattern && formList != null) {
                matched = matchSCElements(pattern, formList, literals, bindings, ellipsisBindings, 0, 0);
            } else if (rawPattern instanceof SchemeSymbol sym && !literals.contains(sym.name()) && !sym.name().equals("_")) {
                bindings.put(sym.name(), form);
                matched = true;
            } else {
                matched = false;
            }

            if (matched) {
                Set<String> patVars = new HashSet<>(bindings.keySet());
                patVars.addAll(ellipsisBindings.keySet());
                Environment defEnv = currentMacroDefEnv != null ? currentMacroDefEnv : env;
                SyntaxCaseContext ctx = new SyntaxCaseContext(bindings, ellipsisBindings, patVars, defEnv);

                if (clause.size() == 3) {
                    syntaxCaseStack.push(ctx);
                    try {
                        Object fenderResult = evalFn.eval(clause.get(1), env);
                        if (fenderResult.equals(Boolean.FALSE)) continue;
                    } finally {
                        syntaxCaseStack.pop();
                    }
                    syntaxCaseStack.push(ctx);
                    try {
                        return evalFn.eval(clause.get(2), env);
                    } finally {
                        syntaxCaseStack.pop();
                    }
                } else {
                    syntaxCaseStack.push(ctx);
                    try {
                        return evalFn.eval(clause.get(1), env);
                    } finally {
                        syntaxCaseStack.pop();
                    }
                }
            }
        }
        throw new EvalError("syntax-case: no matching pattern");
    }

    @SuppressWarnings("unchecked")
    Object evalSyntax(List<Object> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("syntax: bad syntax");
        Object template = deepUnwrap(args.get(0));

        if (syntaxCaseStack.isEmpty()) {
            return new SyntaxObject(template, env);
        }
        SyntaxCaseContext ctx = syntaxCaseStack.peek();

        if (template instanceof SchemeSymbol sym && ctx.patternVars.contains(sym.name())) {
            Object val;
            if (ctx.bindings.containsKey(sym.name())) {
                val = ctx.bindings.get(sym.name());
            } else {
                val = ctx.ellipsisBindings.get(sym.name());
            }
            return new SyntaxObject(val, ctx.defEnv);
        }

        Map<String, String> renames = new HashMap<>();
        collectSyntaxNonPatternSymbols(template, ctx.patternVars, renames);

        for (Map.Entry<String, String> entry : renames.entrySet()) {
            try {
                Object val = ctx.defEnv.lookup(entry.getKey());
                ctx.defEnv.define(entry.getValue(), val);
            } catch (EvalError ignored) {
            }
        }

        return expandSyntaxTemplate(template, ctx.bindings, ctx.ellipsisBindings, renames);
    }

    @SuppressWarnings("unchecked")
    Object evalWithSyntax(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("with-syntax: bad syntax");
        List<?> bindingList = (List<?>) unwrap(args.get(0));

        SyntaxCaseContext parentCtx = syntaxCaseStack.isEmpty() ? null : syntaxCaseStack.peek();
        Map<String, Object> newBindings = parentCtx != null ? new HashMap<>(parentCtx.bindings) : new HashMap<>();
        Map<String, List<Object>> newEllipsis = parentCtx != null ? new HashMap<>(parentCtx.ellipsisBindings) : new HashMap<>();
        Set<String> newPatVars = parentCtx != null ? new HashSet<>(parentCtx.patternVars) : new HashSet<>();
        Environment defEnv = parentCtx != null ? parentCtx.defEnv : (currentMacroDefEnv != null ? currentMacroDefEnv : env);

        for (Object binding : bindingList) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            Object val = evalFn.eval(b.get(1), env);
            if (val instanceof SyntaxObject so) val = so.datum();
            newBindings.put(varName, val);
            newPatVars.add(varName);
        }

        SyntaxCaseContext ctx = new SyntaxCaseContext(newBindings, newEllipsis, newPatVars, defEnv);
        syntaxCaseStack.push(ctx);
        try {
            Object result = VOID;
            for (int i = 1; i < args.size(); i++) {
                result = evalFn.eval(args.get(i), env);
            }
            return result;
        } finally {
            syntaxCaseStack.pop();
        }
    }

    @SuppressWarnings("unchecked")
    private boolean matchSCElements(List<?> pattern, List<?> form, List<String> literals,
            Map<String, Object> bindings, Map<String, List<Object>> ellipsisBindings,
            int pi, int fi) {
        while (pi < pattern.size()) {
            boolean hasEllipsis = pi + 1 < pattern.size() && isEllipsisSym(pattern.get(pi + 1));

            if (hasEllipsis) {
                Object patElem = pattern.get(pi);
                int remainingAfter = 0;
                for (int k = pi + 2; k < pattern.size(); k++) {
                    if (!isEllipsisSym(pattern.get(k))) remainingAfter++;
                }
                int endFi = form.size() - remainingAfter;

                if (patElem instanceof SchemeSymbol sym && !sym.name().equals("_") && !literals.contains(sym.name())) {
                    List<Object> collected = new ArrayList<>();
                    while (fi < endFi) {
                        collected.add(form.get(fi));
                        fi++;
                    }
                    ellipsisBindings.put(sym.name(), collected);
                } else {
                    fi = endFi;
                }
                pi += 2;
            } else {
                if (fi >= form.size()) return false;
                Object patElem = pattern.get(pi);
                Object formElem = form.get(fi);

                if (patElem instanceof SchemeSymbol sym) {
                    if (sym.name().equals("_")) {
                        // Wildcard
                    } else if (literals.contains(sym.name())) {
                        if (!(formElem instanceof SchemeSymbol fs) || !fs.name().equals(sym.name()))
                            return false;
                    } else {
                        bindings.put(sym.name(), formElem);
                    }
                } else if (patElem instanceof List<?> subPat) {
                    List<Object> subForm;
                    if (formElem instanceof List<?> jl) {
                        subForm = (List<Object>) jl;
                    } else if (formElem instanceof SchemePair || formElem instanceof SchemeNil) {
                        subForm = schemePairToJavaList(formElem);
                    } else {
                        return false;
                    }
                    if (!matchSCElements(subPat, subForm, literals, bindings, ellipsisBindings, 0, 0))
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

    private boolean isEllipsisSym(Object o) {
        return o instanceof SchemeSymbol s && s.name().equals("...");
    }

    List<Object> schemePairToJavaList(Object obj) {
        List<Object> result = new ArrayList<>();
        Object cur = obj;
        while (cur instanceof SchemePair p) {
            result.add(p.car);
            cur = p.cdr;
        }
        return result;
    }

    private Object expandSyntaxTemplate(Object template, Map<String, Object> bindings,
            Map<String, List<Object>> ellipsisBindings, Map<String, String> renames) {
        if (template instanceof SchemeSymbol sym) {
            String n = sym.name();
            if (bindings.containsKey(n)) return bindings.get(n);
            if (renames.containsKey(n)) return new SchemeSymbol(renames.get(n));
            return sym;
        }
        if (template instanceof List<?> list) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < list.size(); i++) {
                if (i + 1 < list.size() && isEllipsisSym(list.get(i + 1))) {
                    Object elemTemplate = list.get(i);
                    Set<String> usedEllipsis = new HashSet<>();
                    findEllipsisVarsInTemplate(elemTemplate, ellipsisBindings, usedEllipsis);
                    if (!usedEllipsis.isEmpty()) {
                        String firstVar = usedEllipsis.iterator().next();
                        int count = ellipsisBindings.get(firstVar).size();
                        for (int j = 0; j < count; j++) {
                            Map<String, Object> iterBindings = new HashMap<>(bindings);
                            for (String var : usedEllipsis) {
                                iterBindings.put(var, ellipsisBindings.get(var).get(j));
                            }
                            result.add(expandSyntaxTemplate(elemTemplate, iterBindings, ellipsisBindings, renames));
                        }
                    }
                    i++; // skip ellipsis
                } else {
                    result.add(expandSyntaxTemplate(list.get(i), bindings, ellipsisBindings, renames));
                }
            }
            return result;
        }
        return template;
    }

    private void findEllipsisVarsInTemplate(Object template, Map<String, List<Object>> ellipsisBindings,
            Set<String> result) {
        if (template instanceof SchemeSymbol sym) {
            if (ellipsisBindings.containsKey(sym.name())) result.add(sym.name());
        } else if (template instanceof List<?> list) {
            for (Object elem : list) findEllipsisVarsInTemplate(elem, ellipsisBindings, result);
        }
    }

    private void collectSyntaxNonPatternSymbols(Object template, Set<String> patVars,
            Map<String, String> renames) {
        if (template instanceof SchemeSymbol sym) {
            String n = sym.name();
            if (!patVars.contains(n) && !SYNTAX_SPECIAL_FORMS.contains(n)
                    && !n.equals("...") && !renames.containsKey(n)) {
                renames.put(n, syntaxGensym(n));
            }
        } else if (template instanceof List<?> list) {
            if (!list.isEmpty() && list.get(0) instanceof SchemeSymbol qs && qs.name().equals("quote")) {
                return;
            }
            for (Object elem : list) {
                collectSyntaxNonPatternSymbols(elem, patVars, renames);
            }
        }
    }
}
