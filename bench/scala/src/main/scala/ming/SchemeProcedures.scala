package ming

import SchemeInterpreter.Expr
import SchemeInterpreter.Value
import SchemeInterpreterSyntax.readParams

private[ming] object SchemeProcedures:

  final case class PreparedUserProcedure(
    body: List[Expr],
    env: Env,
    macros: MacroScope
  )

  def evalCaseLambda(args: List[Expr], env: Env, macros: MacroScope, pos: SourcePos): Value.CaseLambda =
    if args.isEmpty then throw EvalError.at(pos, "case-lambda requires at least one clause")
    Value.CaseLambda(args.map(parseCaseLambdaClause), env, macros)

  def selectCaseLambdaClause(
    clauses: List[Value.CaseLambdaClause],
    actual: Int,
    pos: SourcePos
  ): Value.CaseLambdaClause =
    clauses
      .collectFirst { case clause if arityMatches(clause.params, actual) => clause }
      .getOrElse {
        throw EvalError.at(pos, s"case-lambda has no matching clause for $actual arguments")
      }

  private def parseCaseLambdaClause(expr: Expr): Value.CaseLambdaClause =
    expr match
      case Expr.ListExpr(formals :: body, _) if body.nonEmpty =>
        Value.CaseLambdaClause(readParams(formals), body)
      case Expr.ListExpr(_ :: Nil, clausePos) =>
        throw EvalError.at(clausePos, "case-lambda clause requires a body")
      case Expr.ListExpr(Nil, clausePos) =>
        throw EvalError.at(clausePos, "invalid case-lambda clause")
      case other =>
        throw EvalError.at(other.pos, "invalid case-lambda clause")

  def applyUserProcedure(
    params: LambdaParams,
    body: List[Expr],
    closureEnv: Env,
    closureMacros: MacroScope,
    args: List[Value],
    pos: SourcePos,
    context: String
  ): Value =
    val prepared = prepareUserProcedure(params, body, closureEnv, closureMacros, args, pos, context)
    SchemeInterpreter.evalSequence(prepared.body, prepared.env, prepared.macros)

  def prepareUserProcedure(
    params: LambdaParams,
    body: List[Expr],
    closureEnv: Env,
    closureMacros: MacroScope,
    args: List[Value],
    pos: SourcePos,
    context: String
  ): PreparedUserProcedure =
    requireArity(params, args.length, pos, context)
    val callEnv    = Env.child(closureEnv, buildBindings(params, args))
    val callMacros = MacroScope.child(closureMacros)
    PreparedUserProcedure(body, callEnv, callMacros)

  private def buildBindings(params: LambdaParams, args: List[Value]): List[(String, Value)] =
    val minimum = params.required.length
    params.required.zip(args.take(minimum)) ++ params.rest.map { restName =>
      restName -> Value.list(args.drop(minimum))
    }

  private def arityMatches(params: LambdaParams, actual: Int): Boolean =
    val minimum = params.required.length
    params.rest match
      case None    => actual == minimum
      case Some(_) => actual >= minimum

  private def requireArity(
    params: LambdaParams,
    actual: Int,
    pos: SourcePos,
    context: String
  ): Unit =
    val minimum = params.required.length
    params.rest match
      case None if actual != minimum =>
        throw EvalError.at(pos, s"$context expected $minimum arguments, got $actual")
      case Some(_) if actual < minimum =>
        throw EvalError.at(pos, s"$context expected at least $minimum arguments, got $actual")
      case _ =>
