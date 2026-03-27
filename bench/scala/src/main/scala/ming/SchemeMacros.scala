package ming

private[ming] trait SyntaxTransformer:
  def expand(invocation: Expr.ListExpr): Expr

private[ming] object SchemeMacros:

  def parseSyntaxRules(
    name: String,
    transformerExpr: Expr,
    definitionEnv: Env,
    pos: SourcePos
  ): SyntaxTransformer =
    transformerExpr match
      case Expr.ListExpr(
            Expr.Symbol("syntax-rules", _) :: Expr.ListExpr(literalExprs, _) :: rules,
            _
          ) if rules.nonEmpty =>
        val literalNames = literalExprs.map(parseLiteralName)
        val parsedRules  = rules.map(parseRule(name, _, pos))
        SyntaxRulesTransformer(name, literalNames.toSet, parsedRules, definitionEnv)
      case _ =>
        throw EvalError.at(pos, "define-syntax expects a syntax-rules transformer")

  private def parseLiteralName(expr: Expr): String =
    expr match
      case Expr.Symbol(literal, _) => literal
      case other =>
        throw EvalError.at(other.pos, "syntax-rules literals must be symbols")

  private def parseRule(name: String, expr: Expr, pos: SourcePos): SyntaxRule =
    expr match
      case Expr.ListExpr(pattern :: template :: Nil, _) if startsWithKeyword(pattern, name) =>
        SyntaxRule(pattern, template)
      case Expr.ListExpr(pattern :: _ :: Nil, _) =>
        throw EvalError.at(pattern.pos, s"syntax-rules pattern for $name must start with $name")
      case _ =>
        throw EvalError.at(pos, "each syntax-rules clause must contain a pattern and template")

  private def startsWithKeyword(pattern: Expr, name: String): Boolean =
    pattern match
      case Expr.ListExpr(Expr.Symbol(keyword, _) :: _, _) => keyword == name
      case _                                              => false
