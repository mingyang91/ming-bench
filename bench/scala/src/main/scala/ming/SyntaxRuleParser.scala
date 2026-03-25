package ming

private[ming] object SyntaxRuleParser:
  import SchemeInterpreter.Expr

  def parse(name: String, expr: Expr, env: Env, macros: MacroScope): SyntaxRules.Transformer =
    expr match
      case Expr.ListExpr(Expr.Symbol("syntax-rules", _) :: rest, _) =>
        rest match
          case Expr.ListExpr(literalExprs, _) :: ruleExprs if ruleExprs.nonEmpty =>
            val literals = literalExprs.map {
              case Expr.Symbol(literal, _) => literal
              case other =>
                throw EvalError.at(other.pos, "syntax-rules literals must be identifiers")
            }.toSet

            val rules = ruleExprs.map(parseRule)
            new SyntaxTransformer(name, literals, rules, env, macros, SyntaxFreshIds.next())

          case _ =>
            throw EvalError.at(expr.pos, "invalid syntax-rules")

      case _ =>
        throw EvalError.at(expr.pos, "define-syntax requires syntax-rules")

  private def parseRule(expr: Expr): SyntaxRules.Rule =
    expr match
      case Expr.ListExpr(List(pattern: Expr.ListExpr, template), _) if pattern.items.nonEmpty =>
        pattern.items.head match
          case Expr.Symbol(_, _) =>
            SyntaxRules.Rule(pattern, template)
          case other =>
            throw EvalError.at(other.pos, "syntax-rules pattern must start with an identifier")

      case Expr.ListExpr(List(pattern: Expr.ListExpr, _), _) =>
        throw EvalError.at(pattern.pos, "syntax-rules pattern cannot be empty")

      case _ =>
        throw EvalError.at(expr.pos, "invalid syntax-rules clause")
