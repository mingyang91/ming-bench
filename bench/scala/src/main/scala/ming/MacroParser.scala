package ming

private[ming] object MacroParser:

  def parse(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): (String, SyntaxRulesMacro) =
    arguments match
      case SymbolExpr(name, _) :: transformerExpression :: Nil =>
        name -> parseSyntaxRules(name, transformerExpression, position, env)
      case _ =>
        SchemeFailure.raise(
          "define-syntax expected (define-syntax name (syntax-rules ...))",
          position
        )

  def parseSyntaxRules(
    name: String,
    expression: Expr,
    position: Position,
    env: Environment
  ): SyntaxRulesMacro =
    expression match
      case ListExpr(SymbolExpr("syntax-rules", _) :: literalsExpression :: ruleExpressions, _)
          if ruleExpressions.nonEmpty =>
        val literals = parseLiteralIdentifiers(literalsExpression, position)
        val rules    = ruleExpressions.map(ruleExpression => parseRule(ruleExpression, position))
        SyntaxRulesMacro(name, literals, rules, env)
      case _ =>
        SchemeFailure.raise(
          "define-syntax expected a syntax-rules transformer",
          position
        )

  private def parseLiteralIdentifiers(expression: Expr, position: Position): Set[String] =
    expression match
      case ListExpr(literalExpressions, _) =>
        literalExpressions
          .map:
            case SymbolExpr(name, _) => name
            case other =>
              SchemeFailure.raise(
                s"syntax-rules expected literal identifiers, got ${other.getClass.getSimpleName}",
                other.position
              )
          .toSet
      case _ =>
        SchemeFailure.raise("syntax-rules expected a literal identifier list", position)

  private def parseRule(expression: Expr, position: Position): SyntaxRule =
    expression match
      case ListExpr(List(pattern, template), _) =>
        SyntaxRule(pattern, template)
      case _ =>
        SchemeFailure.raise("syntax-rules expected rules of the form (pattern template)", position)
