package ming

private[ming] object MacroExpander:

  def parse(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): (String, SyntaxRulesMacro) =
    MacroParser.parse(arguments, position, env)

  def expand(application: ListExpr, macroDefinition: SyntaxRulesMacro): MacroExpansion =
    MacroMatcher.matchRule(application, macroDefinition) match
      case Some((rule, bindings)) =>
        MacroInstantiator.instantiate(rule.template, bindings, macroDefinition)
      case None =>
        SchemeFailure.raise(
          s"no syntax-rules clause matched ${macroDefinition.name}",
          application.position
        )
