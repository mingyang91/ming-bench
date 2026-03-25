package ming;

import java.util.ArrayList;
import java.util.List;

final class ProcedureSupport {
    private ProcedureSupport() {
    }

    static ParameterSpec parseParameters(Expr parameterExpr) throws EvalError {
        if (parameterExpr instanceof ListExpr listExpr) {
            return parseParameterList(listExpr.elements(), listExpr.pos());
        }
        if (parameterExpr instanceof SymbolExpr symbolExpr) {
            return new ParameterSpec(List.of(), symbolExpr.name());
        }
        throw error("'lambda' parameters must be a list or a symbol", parameterExpr.pos());
    }

    static CaseLambdaClause parseCaseLambdaClause(Expr clauseExpr) throws EvalError {
        if (!(clauseExpr instanceof ListExpr clauseList)) {
            throw error("'case-lambda' clauses must be lists", clauseExpr.pos());
        }

        List<Expr> clauseElements = clauseList.elements();
        if (clauseElements.size() < 2) {
            throw error("'case-lambda' clauses must contain parameters and a body",
                    clauseList.pos());
        }

        ParameterSpec parameters = parseParameters(clauseElements.getFirst());
        return new CaseLambdaClause(parameters.required(), parameters.rest(),
                List.copyOf(clauseElements.subList(1, clauseElements.size())));
    }

    static Environment bindCall(Environment procedureEnvironment, List<String> parameters,
                                String restParameter, List<Value> arguments, SourcePos pos)
            throws EvalError {
        if (!matchesArity(parameters.size(), restParameter, arguments.size())) {
            throw error("wrong number of arguments", pos);
        }

        Environment callEnvironment = new Environment(procedureEnvironment);
        for (int i = 0; i < parameters.size(); i++) {
            callEnvironment.define(parameters.get(i), arguments.get(i));
        }
        if (restParameter != null) {
            callEnvironment.define(restParameter,
                    buildList(arguments.subList(parameters.size(), arguments.size())));
        }
        return callEnvironment;
    }

    static boolean matchesArity(int requiredCount, String restParameter, int argumentCount) {
        if (restParameter == null) {
            return argumentCount == requiredCount;
        }
        return argumentCount >= requiredCount;
    }

    static ParameterSpec parseParameterList(List<Expr> parameterExprs, SourcePos pos)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;
        boolean sawDot = false;

        for (int i = 0; i < parameterExprs.size(); i++) {
            Expr parameterExpr = parameterExprs.get(i);
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error("parameters must be symbols", parameterExpr.pos());
            }

            if (".".equals(symbolExpr.name())) {
                if (sawDot || i == parameterExprs.size() - 1) {
                    throw error("invalid dotted parameter list", symbolExpr.pos());
                }
                sawDot = true;
                continue;
            }

            if (sawDot) {
                if (i != parameterExprs.size() - 1) {
                    throw error("rest parameter must be last", parameterExpr.pos());
                }
                restParameter = symbolExpr.name();
                return new ParameterSpec(List.copyOf(parameters), restParameter);
            }

            parameters.add(symbolExpr.name());
        }

        if (sawDot) {
            throw error("invalid dotted parameter list", pos);
        }

        return new ParameterSpec(List.copyOf(parameters), null);
    }

    private static Value buildList(List<Value> values) {
        Value result = EmptyListValue.INSTANCE;
        for (int i = values.size() - 1; i >= 0; i--) {
            result = new PairValue(values.get(i), result);
        }
        return result;
    }

    private static EvalError error(String message, SourcePos pos) {
        return new EvalError(message, pos.line(), pos.column());
    }
}

record ParameterSpec(List<String> required, String rest) {
}
