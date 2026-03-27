package ming

final private[ming] case class SyntaxCaseTransformer(
  name: String,
  parameter: String,
  body: List[Expr],
  definitionEnv: Env
) extends SyntaxTransformer:

  override def expand(invocation: Expr.ListExpr): Expr =
    SyntaxTransformerEvaluator.expand(name, parameter, body, definitionEnv, invocation)

private[ming] object SyntaxTransformerEvaluator:

  def expand(
    name: String,
    parameter: String,
    body: List[Expr],
    definitionEnv: Env,
    invocation: Expr.ListExpr
  ): Expr =
    val context = SyntaxTransformerRuntime.initialContext(parameter, invocation, definitionEnv)
    SyntaxTransformerRuntime.requireSyntax(
      SyntaxTransformerRuntime.evalBody(body, context),
      invocation.pos,
      s"syntax transformer for $name"
    )
