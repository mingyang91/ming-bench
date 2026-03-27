package ming

private[ming] object SpecialFormProcedureEvaluator:

  final private case class ParameterSpecification(
    parameters: List[String],
    restParameter: Option[String]
  )

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case parametersExpression :: body if body.nonEmpty =>
        buildClosure(parametersExpression, body, env, None, position)
      case _ =>
        SchemeFailure.raise("lambda expected a parameter list and body", position)

  def evalCaseLambda(
    arguments: List[Expr],
    _position: Position,
    env: Environment
  ): Value =
    CaseLambdaValue(arguments.map(buildCaseLambdaClause(_, env)))

  def buildClosure(
    parametersExpression: Expr,
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val parameterSpecification = ParameterSpecification.fromExpr(parametersExpression, position)
    ClosureValue(
      parameterSpecification.parameters,
      parameterSpecification.restParameter,
      body,
      env,
      name
    )

  def buildClosure(
    parameterExpressions: List[Expr],
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val parameterSpecification = ParameterSpecification.fromList(parameterExpressions, position)
    ClosureValue(
      parameterSpecification.parameters,
      parameterSpecification.restParameter,
      body,
      env,
      name
    )

  private def buildCaseLambdaClause(
    clauseExpression: Expr,
    env: Environment
  ): ClosureValue =
    clauseExpression match
      case ListExpr(parametersExpression :: body, clausePosition) if body.nonEmpty =>
        buildClosure(parametersExpression, body, env, None, clausePosition)
      case ListExpr(_ :: Nil, clausePosition) =>
        SchemeFailure.raise("case-lambda clause expected a body", clausePosition)
      case _ =>
        SchemeFailure.raise(
          "case-lambda expected clauses of the form ((args) body ...)",
          clauseExpression.position
        )

  private object ParameterSpecification:

    def fromExpr(
      parametersExpression: Expr,
      position: Position
    ): ParameterSpecification =
      parametersExpression match
        case SymbolExpr(name, _) =>
          ParameterSpecification(Nil, Some(name))
        case ListExpr(parameterExpressions, _) =>
          fromList(parameterExpressions, position)
        case _ =>
          SchemeFailure.raise("lambda expected a parameter list or symbol", position)

    def fromList(
      parameterExpressions: List[Expr],
      position: Position
    ): ParameterSpecification =
      parameterExpressions.span:
        case SymbolExpr(".", _) => false
        case _                  => true
      match
        case (fixedParameters, Nil) =>
          ParameterSpecification(fixedParameters.map(parameterName(_, position)), None)
        case (fixedParameters, List(SymbolExpr(".", _), SymbolExpr(restName, _))) =>
          ParameterSpecification(
            fixedParameters.map(parameterName(_, position)),
            Some(restName)
          )
        case _ =>
          SchemeFailure.raise("parameter list is malformed", position)

    private def parameterName(expression: Expr, position: Position): String =
      expression match
        case SymbolExpr(name, _) =>
          name
        case _ =>
          SchemeFailure.raise("parameters must be symbols", position)
