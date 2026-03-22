package ming

import SchemeValue.*

/** Extracted special forms: let, cond. */
private[ming] object SpecialForms:

  def evalLet(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) = args match
    case SchemeSymbol(name) :: SchemeList(bindings) :: body if body.nonEmpty =>
      evalNamedLet(name, bindings, body, env)
    case SchemeList(bindings) :: body if body.nonEmpty =>
      evalSimpleLet(bindings, body, env)
    case _ => throw new EvalError("let: bad syntax")

  private def evalNamedLet(
    name: String,
    bindings: List[SchemeValue],
    body: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    val (params, inits)         = parseBindings(bindings)
    val (evaledInits, initsOut) = Evaluator.evalArgs(inits, env)
    val recEnv = Env.RecursiveFrame(
      name,
      closure => SchemeLambda(params, body, closure),
      env
    )
    val localEnv             = recEnv.extend(params, evaledInits)
    val (result, _, bodyOut) = Evaluator.evalSequence(body, localEnv)
    (result, env, initsOut + bodyOut)

  private def evalSimpleLet(
    bindings: List[SchemeValue],
    body: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    val (params, inits)         = parseBindings(bindings)
    val (evaledInits, initsOut) = Evaluator.evalArgs(inits, env)
    val localEnv                = env.extend(params, evaledInits)
    val (result, _, bodyOut)    = Evaluator.evalSequence(body, localEnv)
    (result, env, initsOut + bodyOut)

  private def parseBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue]) =
    bindings.map {
      case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
      case other =>
        throw new EvalError(s"let: bad binding: ${other.display}")
    }.unzip

  def evalCond(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    evalCondClauses(args, env, "")

  @scala.annotation.tailrec
  private def evalCondClauses(
    clauses: List[SchemeValue],
    env: Env,
    accOutput: String
  ): (SchemeValue, Env, String) = clauses match
    case Nil => (SchemeVoid, env, accOutput)
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      val (result, _, o) = Evaluator.evalSequence(body, env)
      (result, env, accOutput + o)
    case SchemeList(test :: body) :: rest =>
      val (v, _, testOut) = Evaluator.evalWithEnv(test, env)
      if Evaluator.isFalsy(v) then evalCondClauses(rest, env, accOutput + testOut)
      else if body.isEmpty then (v, env, accOutput + testOut)
      else
        val (result, _, bodyOut) = Evaluator.evalSequence(body, env)
        (result, env, accOutput + testOut + bodyOut)
    case other :: _ =>
      throw new EvalError(s"cond: bad clause: ${other.display}")
