package ming

private[ming] object SpecialFormDefinitionEvaluator:

  def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, _) :: valueExpression :: Nil =>
        evalValueDefine(name, valueExpression, env)
      case ListExpr(SymbolExpr(name, _) :: parameters, _) :: body if body.nonEmpty =>
        evalProcedureDefine(name, parameters, body, env, position)
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
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, symbolPosition) :: valueExpression :: Nil =>
        val value = InterpreterEvaluator.eval(valueExpression, env)
        env.assign(name, value, symbolPosition)
        VoidValue
      case _ =>
        SchemeFailure.raise("set! expected (set! name expr)", position)

  private def evalValueDefine(name: String, valueExpression: Expr, env: Environment): Value =
    env.reserve(name)
    val value = InterpreterEvaluator.eval(valueExpression, env)
    env.define(name, value)
    VoidValue

  private def evalProcedureDefine(
    name: String,
    parameters: List[Expr],
    body: List[Expr],
    env: Environment,
    position: Position
  ): Value =
    env.reserve(name)
    val value = SpecialFormProcedureEvaluator.buildClosure(parameters, body, env, Some(name), position)
    env.define(name, value)
    VoidValue
