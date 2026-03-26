package ming

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

abstract private[ming] class SchemeProcedureApplication:

  protected def evalSequence(expressions: List[Expr], env: Env): Value

  protected def applyClosure(closure: Value.Closure, args: List[Value]): Value =
    applyProcedureBody(
      closure.name.getOrElse("lambda"),
      closure.fixedParams,
      closure.restParam,
      closure.body,
      closure.env,
      args
    )

  protected def applyCaseClosure(caseClosure: Value.CaseClosure, args: List[Value]): Value =
    caseClosure.clauses.find(clauseMatchesArgCount(_, args.length)) match
      case Some(clause) =>
        applyProcedureBody(
          "case-lambda",
          clause.fixedParams,
          clause.restParam,
          clause.body,
          clause.env,
          args
        )
      case None =>
        throw new EvalError(s"case-lambda expected a matching clause for ${args.length} argument(s)")

  private def clauseMatchesArgCount(clause: CaseLambdaClause, argCount: Int): Boolean =
    clause.restParam match
      case Some(_) => argCount >= clause.fixedParams.length
      case None    => argCount == clause.fixedParams.length

  private def applyProcedureBody(
    name: String,
    fixedParams: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value]
  ): Value =
    restParam match
      case Some(_) =>
        requireMinArgCount(name, args, fixedParams.length)
      case None =>
        requireArgCount(name, args, fixedParams.length)

    val callEnv = new Env(Some(closureEnv))
    fixedParams.zip(args).foreach { case (paramName, value) =>
      callEnv.define(paramName, value)
    }
    restParam.foreach { restName =>
      callEnv.define(restName, makeList(args.drop(fixedParams.length)))
    }
    evalSequence(body, callEnv)
