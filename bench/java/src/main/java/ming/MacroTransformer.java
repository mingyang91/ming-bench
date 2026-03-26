package ming;

interface MacroTransformer {
    SchemeExpression expand(Evaluator evaluator, ListExpression invocation) throws EvalError;
}

final class ProcedureMacro implements MacroTransformer {
    private final String name;
    private final ProcedureValue transformer;
    private final SyntaxTemplateContext templateContext;

    ProcedureMacro(
            String name,
            ProcedureValue transformer,
            Environment definitionEnvironment,
            long templateId
    ) {
        this.name = name;
        this.transformer = transformer;
        this.templateContext = new SyntaxTemplateContext(definitionEnvironment, templateId);
    }

    @Override
    public SchemeExpression expand(Evaluator evaluator, ListExpression invocation) throws EvalError {
        return evaluator.expandProcedureMacro(name, transformer, templateContext, invocation);
    }
}
