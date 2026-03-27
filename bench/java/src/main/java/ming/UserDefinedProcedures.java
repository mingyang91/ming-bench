package ming;

import java.util.List;

@FunctionalInterface
interface SequenceEvaluator {
    Value eval(List<Expr> expressions, Environment env) throws EvalError;
}

@FunctionalInterface
interface ListValueBuilder {
    Value build(List<Value> values);
}

record ProcedureRuntime(SequenceEvaluator sequenceEvaluator, ListValueBuilder listValueBuilder) {
    Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        return sequenceEvaluator.eval(expressions, env);
    }

    Value buildList(List<Value> values) {
        return listValueBuilder.build(values);
    }

    EvalError error(SourceLoc loc, String message) {
        return SchemeErrors.at(loc, message);
    }
}

record CaseLambdaClause(
        List<String> parameters,
        String restParameter,
        List<Expr> body
) {
    boolean matches(int argumentCount) {
        if (restParameter == null) {
            return argumentCount == parameters.size();
        }
        return argumentCount >= parameters.size();
    }
}

final class CaseLambdaProcedure implements Value, Procedure, TailCallable {
    private final List<CaseLambdaClause> clauses;
    private final Environment closureEnv;
    private final ProcedureRuntime runtime;

    CaseLambdaProcedure(
            List<CaseLambdaClause> clauses,
            Environment closureEnv,
            ProcedureRuntime runtime
    ) {
        this.clauses = clauses;
        this.closureEnv = closureEnv;
        this.runtime = runtime;
    }

    @Override
    public String render() {
        return "#<procedure:case-lambda>";
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        SequenceTask task = prepareTailCall(arguments, callLoc);
        return runtime.evalSequence(task.expressions(), task.env());
    }

    MachineState invoke(List<Value> arguments, SourceLoc callLoc, Continuation cont)
            throws EvalError {
        SequenceTask task = prepareTailCall(arguments, callLoc);
        return new SequenceState(task.expressions(), task.env(), cont);
    }

    @Override
    public SequenceTask prepareTailCall(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        for (CaseLambdaClause clause : clauses) {
            if (!clause.matches(arguments.size())) {
                continue;
            }
            return prepareClause(clause, arguments);
        }

        throw runtime.error(
                callLoc,
                "case-lambda has no matching clause for " + arguments.size() + " arguments"
        );
    }

    private SequenceTask prepareClause(CaseLambdaClause clause, List<Value> arguments) {
        Environment callEnv = new Environment(closureEnv);
        bindRequiredParameters(callEnv, clause.parameters(), arguments);
        bindRestParameter(callEnv, clause.parameters().size(), clause.restParameter(), arguments);
        return new SequenceTask(clause.body(), callEnv);
    }

    private void bindRequiredParameters(
            Environment callEnv,
            List<String> parameters,
            List<Value> arguments
    ) {
        for (int index = 0; index < parameters.size(); index++) {
            callEnv.define(parameters.get(index), arguments.get(index));
        }
    }

    private void bindRestParameter(
            Environment callEnv,
            int parameterCount,
            String restParameter,
            List<Value> arguments
    ) {
        if (restParameter == null) {
            return;
        }
        callEnv.define(
                restParameter,
                runtime.buildList(arguments.subList(parameterCount, arguments.size()))
        );
    }
}

final class UserProcedure implements Value, Procedure, TailCallable {
    private final String name;
    private final List<String> parameters;
    private final String restParameter;
    private final List<Expr> body;
    private final Environment closureEnv;
    private final ProcedureRuntime runtime;

    UserProcedure(
            String name,
            List<String> parameters,
            String restParameter,
            List<Expr> body,
            Environment closureEnv,
            ProcedureRuntime runtime
    ) {
        this.name = name;
        this.parameters = parameters;
        this.restParameter = restParameter;
        this.body = body;
        this.closureEnv = closureEnv;
        this.runtime = runtime;
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        SequenceTask task = prepareTailCall(arguments, callLoc);
        return runtime.evalSequence(task.expressions(), task.env());
    }

    MachineState invoke(List<Value> arguments, SourceLoc callLoc, Continuation cont)
            throws EvalError {
        SequenceTask task = prepareTailCall(arguments, callLoc);
        return new SequenceState(task.expressions(), task.env(), cont);
    }

    @Override
    public SequenceTask prepareTailCall(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureArity(arguments.size(), callLoc);

        Environment callEnv = new Environment(closureEnv);
        bindRequiredParameters(callEnv, arguments);
        bindRestParameter(callEnv, arguments);
        return new SequenceTask(body, callEnv);
    }

    private void ensureArity(int argumentCount, SourceLoc callLoc) throws EvalError {
        if (restParameter == null && argumentCount != parameters.size()) {
            throw runtime.error(
                    callLoc,
                    name + " expected " + parameters.size() + " arguments but got "
                            + argumentCount
            );
        }
        if (restParameter != null && argumentCount < parameters.size()) {
            throw runtime.error(
                    callLoc,
                    name + " expected at least " + parameters.size() + " arguments but got "
                            + argumentCount
            );
        }
    }

    private void bindRequiredParameters(Environment callEnv, List<Value> arguments) {
        for (int index = 0; index < parameters.size(); index++) {
            callEnv.define(parameters.get(index), arguments.get(index));
        }
    }

    private void bindRestParameter(Environment callEnv, List<Value> arguments) {
        if (restParameter == null) {
            return;
        }
        callEnv.define(
                restParameter,
            runtime.buildList(arguments.subList(parameters.size(), arguments.size()))
        );
    }
}

final class ContinuationProcedure implements Value, Procedure {
    private final Continuation continuation;

    ContinuationProcedure(Continuation continuation) {
        this.continuation = continuation;
    }

    @Override
    public String render() {
        return "#<procedure:continuation>";
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactlyOne(arguments, callLoc);
        return arguments.getFirst();
    }

    MachineState invoke(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactlyOne(arguments, callLoc);
        return new ReturnState(arguments.getFirst(), continuation);
    }

    private void ensureExactlyOne(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        if (arguments.size() != 1) {
            throw SchemeErrors.at(
                    callLoc,
                    "continuation expected 1 argument but got " + arguments.size()
            );
        }
    }
}
