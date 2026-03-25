package ming

final private[ming] class ProcedureMacroTransformer(
  val name: String,
  private val procedure: SchemeInterpreter.Procedure,
  private val definitionEnv: Env,
  private val definitionMacros: MacroScope
) extends SyntaxRules.Transformer:

  import SchemeInterpreter.Expr

  def expand(call: Expr.ListExpr, pos: SourcePos): Expr =
    SchemeSyntax.expandTransformer(procedure, call, pos, definitionEnv, definitionMacros)
