package ming

private[ming] object SpecialFormDefinitionEvaluator:

  def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case SymbolExpr(name, _) :: valueExpression :: Nil =>
        evalValueDefine(name, valueExpression, env, continuation)
      case ListExpr(SymbolExpr(name, _) :: parameters, _) :: body if body.nonEmpty =>
        evalProcedureDefine(name, parameters, body, env, position, continuation)
      case _ =>
        SchemeFailure.raise(
          "define expected (define name expr) or (define (name args) body ...)",
          position
        )

  def evalDefineSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val (name, macroDefinition) = MacroExpander.parse(arguments, position, env)
    env.defineMacro(name, macroDefinition)
    VoidValue

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case SymbolExpr(name, symbolPosition) :: valueExpression :: Nil =>
        InterpreterEvaluator.deferExpr(
          valueExpression,
          env,
          value =>
            env.assign(name, value, symbolPosition)
            InterpreterEvaluator.done(VoidValue, continuation)
        )
      case _ =>
        SchemeFailure.raise("set! expected (set! name expr)", position)

  private def evalValueDefine(
    name: String,
    valueExpression: Expr,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    env.reserve(name)
    InterpreterEvaluator.deferExpr(
      valueExpression,
      env,
      value =>
        env.define(name, value)
        InterpreterEvaluator.done(VoidValue, continuation)
    )

  private def evalProcedureDefine(
    name: String,
    parameters: List[Expr],
    body: List[Expr],
    env: Environment,
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    env.reserve(name)
    val value = SpecialFormProcedureEvaluator.buildClosure(parameters, body, env, Some(name), position)
    env.define(name, value)
    InterpreterEvaluator.done(VoidValue, continuation)
