package ming

import scala.annotation.tailrec

/** Special form evaluators extracted from Interpreter for file size limits. */
object SpecialForms:

  import SchemeValue.*
  import Interpreter.EvalResult

  def evalLetStep(
    args: List[SchemeValue],
    env: Environment
  ): EvalResult =
    args match
      case SchemeSymbol(name) :: SchemeList(bindings) :: body if body.nonEmpty =>
        val (paramNames, initExprs) = bindings.map {
          case SchemeList(List(SchemeSymbol(p), init)) => (p, init)
          case _                                       => throw new EvalError("bad named let binding")
        }.unzip
        val (initVals, initOut) = Interpreter.evalArgs(initExprs, env)
        val lambda              = SchemeLambda(paramNames, body, env, Some(name))
        Interpreter.applyProcStep(lambda, initVals, env, initOut)
      case SchemeList(bindings) :: body if body.nonEmpty =>
        val letEnv = env.extend(Nil, Nil)
        val bindOut = bindings.foldLeft("") {
          case (accOut, SchemeList(List(SchemeSymbol(name), expr))) =>
            val (v, _, o) = Interpreter.eval(expr, env)
            letEnv.define(name, v)
            accOut + o
          case _ => throw new EvalError("bad let binding")
        }
        Interpreter.evalBodyBounce(body, letEnv, bindOut)
      case _ => throw new EvalError("bad let syntax")

  @tailrec
  def evalCondStep(
    clauses: List[SchemeValue],
    env: Environment,
    accOut: String
  ): EvalResult =
    clauses match
      case Nil => EvalResult.Done(SchemeVoid, env, accOut)
      case SchemeList(SchemeSymbol("else") :: body) :: _ =>
        Interpreter.evalBodyBounce(body, env, accOut)
      case SchemeList(test :: body) :: rest =>
        val (testVal, _, testOut) = Interpreter.eval(test, env)
        testVal match
          case SchemeBool(false) =>
            evalCondStep(rest, env, accOut + testOut)
          case _ =>
            if body.isEmpty then EvalResult.Done(testVal, env, accOut + testOut)
            else Interpreter.evalBodyBounce(body, env, accOut + testOut)
      case _ => throw new EvalError("bad cond syntax")
