package ming;

import java.util.List;

record ProcedureClause(ParameterSpec parameters, List<Expr> body) {
    ProcedureClause {
        body = List.copyOf(body);
    }
}

final class BuiltinProcedure extends ProcedureValue {
    private final String name;
    private final BuiltinAction action;

    BuiltinProcedure(String name, BuiltinAction action) {
        this.name = name;
        this.action = action;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        return action.apply(args);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class UserProcedure extends ProcedureValue {
    private final Evaluator evaluator;
    private final String name;
    private final ParameterSpec parameters;
    private final List<Expr> body;
    private final Environment closureEnv;

    UserProcedure(Evaluator evaluator, String name, ParameterSpec parameters,
                  List<Expr> body, Environment closureEnv) {
        this.evaluator = evaluator;
        this.name = name;
        this.parameters = parameters;
        this.body = List.copyOf(body);
        this.closureEnv = closureEnv;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        return evaluator.applyUserProcedure(displayName(), parameters, body, closureEnv, args);
    }

    String displayName() {
        return name == null ? "lambda" : name;
    }

    ParameterSpec parameters() {
        return parameters;
    }

    List<Expr> body() {
        return body;
    }

    Environment closureEnv() {
        return closureEnv;
    }
}

final class CaseLambdaProcedure extends ProcedureValue {
    private final Evaluator evaluator;
    private final List<ProcedureClause> clauses;
    private final Environment closureEnv;

    CaseLambdaProcedure(Evaluator evaluator, List<ProcedureClause> clauses,
                        Environment closureEnv) {
        this.evaluator = evaluator;
        this.clauses = List.copyOf(clauses);
        this.closureEnv = closureEnv;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        for (ProcedureClause clause : clauses) {
            if (evaluator.matchesArity(clause.parameters(), args.size())) {
                return evaluator.applyUserProcedure("case-lambda", clause.parameters(),
                        clause.body(), closureEnv, args);
            }
        }
        throw new EvalError("wrong number of arguments for case-lambda: got " + args.size());
    }

    List<ProcedureClause> clauses() {
        return clauses;
    }

    Environment closureEnv() {
        return closureEnv;
    }
}

final class CallCcProcedure extends ProcedureValue {
    private final String name;

    CallCcProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class ValuesProcedure extends ProcedureValue {
    private final String name;

    ValuesProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class CallWithValuesProcedure extends ProcedureValue {
    private final String name;

    CallWithValuesProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class DynamicWindProcedure extends ProcedureValue {
    private final String name;

    DynamicWindProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class RaiseProcedure extends ProcedureValue {
    private final String name;

    RaiseProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class WithExceptionHandlerProcedure extends ProcedureValue {
    private final String name;

    WithExceptionHandlerProcedure(String name) {
        this.name = name;
    }

    String name() {
        return name;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError(name + " cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class ContinuationProcedure extends ProcedureValue {
    private final Continuation continuation;
    private final WindFrame windContext;
    private final ExceptionHandlerFrame exceptionHandlerContext;

    ContinuationProcedure(Continuation continuation, WindFrame windContext,
                          ExceptionHandlerFrame exceptionHandlerContext) {
        this.continuation = continuation;
        this.windContext = windContext;
        this.exceptionHandlerContext = exceptionHandlerContext;
    }

    Continuation continuation() {
        return continuation;
    }

    WindFrame windContext() {
        return windContext;
    }

    ExceptionHandlerFrame exceptionHandlerContext() {
        return exceptionHandlerContext;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        throw new EvalError("continuation cannot be applied directly");
    }

    @Override
    public String render() {
        return "#<continuation>";
    }
}
