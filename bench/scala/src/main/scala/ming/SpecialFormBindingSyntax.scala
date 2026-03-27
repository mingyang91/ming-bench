package ming

private[ming] object SpecialFormBindingSyntax:

  final case class Binding(name: String, valueExpression: Expr)

  final case class DoBinding(
    name: String,
    initExpression: Expr,
    stepExpression: Option[Expr],
    position: Position
  )

  final case class DoTestClause(
    testExpression: Expr,
    finalExpressions: List[Expr]
  )

  enum RecursiveLetMode:
    case Parallel
    case Sequential

  def parseBindings(
    bindingsExpression: Expr,
    position: Position,
    formName: String
  ): List[Binding] =
    bindingsExpression match
      case ListExpr(bindings, _) =>
        bindings.map(parseBinding(_, formName))
      case _ =>
        SchemeFailure.raise(s"$formName expected a binding list", position)

  private def parseBinding(binding: Expr, formName: String): Binding =
    binding match
      case ListExpr(List(SymbolExpr(name, _), valueExpression), _) =>
        Binding(name, valueExpression)
      case _ =>
        SchemeFailure.raise(
          s"$formName expected bindings of the form (name expr)",
          binding.position
        )

  def parseDoBindings(
    bindingsExpression: Expr,
    position: Position
  ): List[DoBinding] =
    bindingsExpression match
      case ListExpr(bindings, _) =>
        bindings.map(parseDoBinding)
      case _ =>
        SchemeFailure.raise("do expected a binding list", position)

  private def parseDoBinding(binding: Expr): DoBinding =
    binding match
      case ListExpr(List(SymbolExpr(name, _), initExpression), bindingPosition) =>
        DoBinding(name, initExpression, None, bindingPosition)
      case ListExpr(
            List(SymbolExpr(name, _), initExpression, stepExpression),
            bindingPosition
          ) =>
        DoBinding(name, initExpression, Some(stepExpression), bindingPosition)
      case _ =>
        SchemeFailure.raise(
          "do expected bindings of the form (name init) or (name init step)",
          binding.position
        )

  def parseDoTestClause(testClauseExpression: Expr): DoTestClause =
    testClauseExpression match
      case ListExpr(testExpression :: finalExpressions, _) =>
        DoTestClause(testExpression, finalExpressions)
      case _ =>
        SchemeFailure.raise(
          "do expected a test clause of the form (test expr ...)",
          testClauseExpression.position
        )
