package ming

import scala.annotation.tailrec

object EvalGuard:

  def evalGuardBounce(args: List[Value], env: Env): Eval.Bounce = args match
    case Value.SList(Value.Symbol(exnVar) :: clauses) :: body if body.nonEmpty =>
      Eval.Bounce.Guard(exnVar, clauses, body, env, "")
    case _ => throw new EvalError("bad guard syntax")

  /** Fully evaluate a guard form, using a trampoline to avoid nesting try-catch frames. */
  def evalGuardFull(
    exnVar: String,
    clauses: List[Value],
    body: List[Value],
    env: Env
  ): (Value, String) =
    guardLoop(exnVar, clauses, body, env, "")

  @tailrec
  private def guardLoop(
    exnVar: String,
    clauses: List[Value],
    body: List[Value],
    env: Env,
    accOut: String
  ): (Value, String) =
    guardOnce(exnVar, clauses, body, env) match
      case GuardStep.Done(v, o)              => (v, accOut + o)
      case GuardStep.Next(ev, cl, bd, ge, o) => guardLoop(ev, cl, bd, ge, accOut + o)

  private def guardOnce(
    exnVar: String,
    clauses: List[Value],
    body: List[Value],
    env: Env
  ): GuardStep =
    try resolveInGuard(Eval.evalBodyBounce(body, env, ""))
    catch
      case e: SchemeException =>
        val guardEnv = env.extend(exnVar, e.value)
        val (v, o)   = Eval.resolveBounce(evalGuardClauses(clauses, guardEnv, e))
        GuardStep.Done(v, o)

  @tailrec
  private def resolveInGuard(b: Eval.Bounce): GuardStep = b match
    case Eval.Bounce.Done(v, o) => GuardStep.Done(v, o)
    case Eval.Bounce.More(e, env2, o) =>
      resolveInGuard(Eval.prependOutput(Eval.evalBounce(e, env2), o))
    case g: Eval.Bounce.Guard =>
      GuardStep.Next(g.exnVar, g.clauses, g.body, g.env, g.output)

  private enum GuardStep:
    case Done(value: Value, output: String)
    case Next(exnVar: String, clauses: List[Value], body: List[Value], env: Env, output: String)

  @tailrec
  private def evalGuardClauses(
    clauses: List[Value],
    env: Env,
    exn: SchemeException
  ): Eval.Bounce = clauses match
    case Nil => throw exn
    case Value.SList(Value.Symbol("else") :: body) :: _ =>
      Eval.evalBodyBounce(body, env, "")
    case Value.SList(test :: Nil) :: rest =>
      val (result, o) = Eval.eval(test, env)
      if !Value.isFalsy(result) then Eval.Bounce.Done(result, o)
      else evalGuardClauses(rest, env, exn)
    case Value.SList(test :: body) :: rest =>
      val (result, o) = Eval.eval(test, env)
      if !Value.isFalsy(result) then Eval.prependOutput(Eval.evalBodyBounce(body, env, ""), o)
      else evalGuardClauses(rest, env, exn)
    case other :: _ => throw new EvalError(s"bad guard clause: ${other.display}")

  def evalCaseBounce(args: List[Value], env: Env): Eval.Bounce = args match
    case key :: clauses if clauses.nonEmpty =>
      val (keyVal, o1) = Eval.eval(key, env)
      evalCaseClauses(keyVal, clauses, env, o1)
    case _ => throw new EvalError("bad case syntax")

  @tailrec
  private def evalCaseClauses(
    key: Value,
    clauses: List[Value],
    env: Env,
    out: String
  ): Eval.Bounce = clauses match
    case Nil => Eval.Bounce.Done(Value.Void, out)
    case Value.SList(Value.Symbol("else") :: body) :: _ =>
      Eval.evalBodyBounce(body, env, out)
    case Value.SList(Value.SList(datums) :: body) :: rest =>
      if datums.exists(d => eqvCompare(key, d)) then Eval.evalBodyBounce(body, env, out)
      else evalCaseClauses(key, rest, env, out)
    case other :: _ => throw new EvalError(s"bad case clause: ${other.display}")

  private def eqvCompare(a: Value, b: Value): Boolean = (a, b) match
    case (Value.Integer(x), Value.Integer(y)) => x == y
    case (Value.Bool(x), Value.Bool(y))       => x == y
    case (Value.Symbol(x), Value.Symbol(y))   => x == y
    case (Value.Char(x), Value.Char(y))       => x == y
    case (Value.SList(Nil), Value.SList(Nil)) => true
    case (Value.Void, Value.Void)             => true
    case _                                    => a eq b
