package ming

import RuntimeSupport.*

private[ming] object SpecialFormSyntaxEvaluator:

  final private case class SyntaxCaseClause(
    pattern: Expr,
    fender: Option[Expr],
    body: List[Expr],
    position: Position
  )

  def evalSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case template :: Nil =>
        InterpreterEvaluator.done(SyntaxSupport.instantiate(template, env, position), continuation)
      case _ =>
        SchemeFailure.raise("syntax expected 1 argument(s)", position)

  def evalSyntaxCase(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case inputExpression :: literalsExpression :: clauses if clauses.nonEmpty =>
        val literalIdentifiers = SyntaxSupport.parseLiteralIdentifiers(literalsExpression, position)
        InterpreterEvaluator.deferExpr(
          inputExpression,
          env,
          value =>
            val syntaxObject =
              expectSyntaxObject(
                expectSingleValue(value, "syntax-case", inputExpression.position),
                "syntax-case",
                inputExpression.position
              )
            evalSyntaxCaseClauses(
              syntaxObject,
              literalIdentifiers,
              clauses,
              env,
              continuation
            )
        )
      case _ =>
        SchemeFailure.raise(
          "syntax-case expected a syntax object, literal identifiers, and at least one clause",
          position
        )

  def evalWithSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case bindingsExpression :: body if body.nonEmpty =>
        val bindings = parseWithSyntaxBindings(bindingsExpression, position)
        evalWithSyntaxBindings(bindings, body, env, continuation)
      case _ =>
        SchemeFailure.raise("with-syntax expected a binding list and body", position)

  private def evalSyntaxCaseClauses(
    syntaxObject: SyntaxObjectValue,
    literalIdentifiers: Set[String],
    clauses: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    clauses match
      case Nil =>
        SchemeFailure.raise("syntax-case found no matching clause", syntaxObject.expr.position)
      case clauseExpression :: rest =>
        val clause = parseSyntaxCaseClause(clauseExpression)
        SyntaxCaseMatcher.matchPattern(clause.pattern, syntaxObject.expr, literalIdentifiers) match
          case None =>
            evalSyntaxCaseClauses(syntaxObject, literalIdentifiers, rest, env, continuation)
          case Some(bindings) =>
            val clauseEnv = Environment.child(
              env,
              SyntaxSupport.bindingsToValues(bindings, syntaxObject.expansion.aliases)
            )
            clause.fender match
              case Some(fenderExpression) =>
                InterpreterEvaluator.deferExpr(
                  fenderExpression,
                  clauseEnv,
                  fenderValue =>
                    if isTruthy(fenderValue) then
                      InterpreterEvaluator.deferSequence(clause.body, clauseEnv, continuation)
                    else evalSyntaxCaseClauses(syntaxObject, literalIdentifiers, rest, env, continuation)
                )
              case None =>
                InterpreterEvaluator.deferSequence(clause.body, clauseEnv, continuation)

  private def parseSyntaxCaseClause(expression: Expr): SyntaxCaseClause =
    expression match
      case ListExpr(pattern :: body :: Nil, clausePosition) =>
        SyntaxCaseClause(pattern, None, List(body), clausePosition)
      case ListExpr(pattern :: fender :: body, clausePosition) if body.nonEmpty =>
        SyntaxCaseClause(pattern, Some(fender), body, clausePosition)
      case _ =>
        SchemeFailure.raise(
          "syntax-case expected clauses of the form (pattern template) or (pattern fender template)",
          expression.position
        )

  private def parseWithSyntaxBindings(
    expression: Expr,
    position: Position
  ): List[(Expr, Expr)] =
    expression match
      case ListExpr(bindingExpressions, _) =>
        bindingExpressions.map:
          case ListExpr(List(pattern, valueExpression), _) =>
            pattern -> valueExpression
          case other =>
            SchemeFailure.raise(
              "with-syntax expected bindings of the form (pattern expr)",
              other.position
            )
      case _ =>
        SchemeFailure.raise("with-syntax expected a binding list", position)

  private def evalWithSyntaxBindings(
    bindings: List[(Expr, Expr)],
    body: List[Expr],
    env: Environment,
    continuation: Continuation,
    collected: Map[String, PatternBinding] = Map.empty
  ): EvaluationStep =
    bindings match
      case Nil =>
        val withSyntaxEnv = Environment.child(env, SyntaxSupport.bindingsToValues(collected))
        InterpreterEvaluator.deferSequence(body, withSyntaxEnv, continuation)
      case (pattern, valueExpression) :: rest =>
        InterpreterEvaluator.deferExpr(
          valueExpression,
          env,
          value =>
            val syntaxObject =
              expectSyntaxObject(
                expectSingleValue(value, "with-syntax", valueExpression.position),
                "with-syntax",
                valueExpression.position
              )
            SyntaxCaseMatcher.matchPattern(pattern, syntaxObject.expr, Set.empty) match
              case Some(boundValues) =>
                val merged =
                  SyntaxSupport.mergeBindings(
                    collected,
                    boundValues,
                    pattern.position,
                    "with-syntax"
                  )
                evalWithSyntaxBindings(rest, body, env, continuation, merged)
              case None =>
                SchemeFailure.raise("with-syntax pattern did not match", pattern.position)
        )
