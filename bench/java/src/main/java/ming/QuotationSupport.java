package ming;

import static ming.EvaluatorSupport.listValue;
import static ming.EvaluatorSupport.requireExactArgs;
import static ming.EvaluatorSupport.requireProperList;
import static ming.RuntimeConstants.EMPTY_LIST;
import static ming.RuntimeConstants.boolValue;
import static ming.ValueSupport.exactValue;
import static ming.ValueSupport.immutableString;

import java.util.ArrayList;
import java.util.List;

final class QuotationSupport {
    @FunctionalInterface
    interface ExprEvaluator {
        Value eval(Expr expr, Env env) throws EvalError;
    }

    private QuotationSupport() {
    }

    static Value evalQuote(List<Expr> arguments) throws EvalError {
        requireExactArgs("quote", arguments, 1);
        return quoteToValue(arguments.get(0));
    }

    static Value evalQuasiquote(List<Expr> arguments, Env env, ExprEvaluator evaluator)
            throws EvalError {
        requireExactArgs("quasiquote", arguments, 1);
        return evalQuasiquote(arguments.get(0), env, 0, evaluator);
    }

    static Value quoteToValue(Expr expr) {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> exactValue(
                    rationalExpr.numerator(), rationalExpr.denominator());
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> boolValue(boolExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case StringExpr stringExpr -> immutableString(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case VectorExpr vectorExpr -> new VectorValue(quoteElements(vectorExpr.elements()));
            case ListExpr listExpr -> quoteListToValue(listExpr.elements());
        };
    }

    private static Value evalQuasiquote(Expr expr, Env env, int depth, ExprEvaluator evaluator)
            throws EvalError {
        if (expr instanceof ListExpr listExpr) {
            String operatorName = quasiquoteFormName(listExpr);
            if ("unquote".equals(operatorName)) {
                requireExactArgs("unquote", listExpr.elements().subList(1,
                        listExpr.elements().size()), 1);
                if (depth == 0) {
                    return evaluator.eval(listExpr.elements().get(1), env);
                }
                return listValue(List.of(new SymbolValue("unquote"),
                        evalQuasiquote(listExpr.elements().get(1), env, depth - 1, evaluator)));
            }
            if ("unquote-splicing".equals(operatorName)) {
                requireExactArgs("unquote-splicing", listExpr.elements().subList(1,
                        listExpr.elements().size()), 1);
                if (depth == 0) {
                    throw new EvalError(
                            "unquote-splicing can only appear within a list or vector template");
                }
                return listValue(List.of(new SymbolValue("unquote-splicing"),
                        evalQuasiquote(listExpr.elements().get(1), env, depth - 1, evaluator)));
            }
            if ("quasiquote".equals(operatorName)) {
                requireExactArgs("quasiquote", listExpr.elements().subList(1,
                        listExpr.elements().size()), 1);
                return listValue(List.of(new SymbolValue("quasiquote"),
                        evalQuasiquote(listExpr.elements().get(1), env, depth + 1, evaluator)));
            }
            return evalQuasiquoteList(listExpr.elements(), env, depth, evaluator);
        }
        if (expr instanceof VectorExpr vectorExpr) {
            return evalQuasiquoteVector(vectorExpr.elements(), env, depth, evaluator);
        }
        return quoteToValue(expr);
    }

    private static Value evalQuasiquoteList(List<Expr> expressions, Env env, int depth,
            ExprEvaluator evaluator) throws EvalError {
        int dottedTailIndex = dottedTailIndex(expressions);
        int prefixCount = dottedTailIndex >= 0 ? dottedTailIndex : expressions.size();
        List<Value> prefixValues = new ArrayList<>();
        for (int index = 0; index < prefixCount; index++) {
            Expr expression = expressions.get(index);
            if (depth == 0 && isQuasiquoteSplice(expression)) {
                prefixValues.addAll(requireProperList(
                        evaluator.eval(quasiquoteOperand((ListExpr) expression), env),
                        "unquote-splicing"));
                continue;
            }
            prefixValues.add(evalQuasiquote(expression, env, depth, evaluator));
        }

        Value result = dottedTailIndex >= 0
                ? evalQuasiquote(expressions.get(expressions.size() - 1), env, depth, evaluator)
                : EMPTY_LIST;
        for (int index = prefixValues.size() - 1; index >= 0; index--) {
            result = new PairValue(prefixValues.get(index), result);
        }
        return result;
    }

    private static Value evalQuasiquoteVector(List<Expr> expressions, Env env, int depth,
            ExprEvaluator evaluator) throws EvalError {
        List<Value> elements = new ArrayList<>();
        for (Expr expression : expressions) {
            if (depth == 0 && isQuasiquoteSplice(expression)) {
                elements.addAll(requireProperList(
                        evaluator.eval(quasiquoteOperand((ListExpr) expression), env),
                        "unquote-splicing"));
                continue;
            }
            elements.add(evalQuasiquote(expression, env, depth, evaluator));
        }
        return new VectorValue(elements);
    }

    private static Value quoteListToValue(List<Expr> expressions) {
        int dottedTailIndex = dottedTailIndex(expressions);
        if (dottedTailIndex < 0) {
            return listValue(quoteElements(expressions));
        }

        Value result = quoteToValue(expressions.get(expressions.size() - 1));
        for (int index = dottedTailIndex - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(expressions.get(index)), result);
        }
        return result;
    }

    private static int dottedTailIndex(List<Expr> expressions) {
        int dottedTailIndex = -1;
        for (int index = 0; index < expressions.size(); index++) {
            if (!".".equals(symbolName(expressions.get(index)))) {
                continue;
            }
            if (dottedTailIndex != -1) {
                return -1;
            }
            dottedTailIndex = index;
        }

        if (dottedTailIndex <= 0 || dottedTailIndex != expressions.size() - 2) {
            return -1;
        }
        return dottedTailIndex;
    }

    private static List<Value> quoteElements(List<Expr> expressions) {
        List<Value> values = new ArrayList<>(expressions.size());
        for (Expr expression : expressions) {
            values.add(quoteToValue(expression));
        }
        return values;
    }

    private static boolean isQuasiquoteSplice(Expr expr) {
        return expr instanceof ListExpr listExpr
                && "unquote-splicing".equals(quasiquoteFormName(listExpr))
                && listExpr.elements().size() == 2;
    }

    private static Expr quasiquoteOperand(ListExpr listExpr) {
        return listExpr.elements().get(1);
    }

    private static String quasiquoteFormName(ListExpr listExpr) {
        if (listExpr.elements().isEmpty()) {
            return null;
        }
        return symbolName(listExpr.elements().get(0));
    }

    private static String symbolName(Expr expr) {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        return null;
    }
}
