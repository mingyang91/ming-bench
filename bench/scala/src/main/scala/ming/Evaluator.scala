package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. */
object Evaluator:

  /** Internal result type for trampoline-based TCO. */
  sealed private[ming] trait EvalResult
  private[ming] case class Done(value: Value, env: Env, out: String)  extends EvalResult
  private[ming] case class Bounce(expr: Value, env: Env, out: String) extends EvalResult

  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env                  = Builtins.defaultEnv.define("__callcc_replay__", Value.NilVal)
    val (lastVal, _, output) = evalTopLevel(exprs, env, "")
    (lastVal.display, output)

  private def evalTopLevel(
    exprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    try evalAll(exprs, env, out)
    catch
      case ci: ContinuationInvoked =>
        val capturedEnv = ci.envThunk()
        capturedEnv.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
        evalTopLevel(ci.remaining, capturedEnv, ci.capturedOut)

  private[ming] def evalAll(
    exprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    exprs match
      case Nil => (Value.VoidVal, env, out)
      case head :: Nil =>
        try eval(head, env, out)
        catch
          case setup: CallCCSetup =>
            Continuations.replayCallCC(setup, head, Nil, env, out)
          case ci: ContinuationInvoked =>
            Continuations.resumeContinuation(ci)
      case head :: tail =>
        try
          val (_, newEnv, out2) = eval(head, env, out)
          val nextEnv = head match
            case Value.PairVal(Value.Symbol("define", _), _, _) =>
              patchClosures(newEnv)
            case Value.PairVal(Value.Symbol("define-syntax", _), _, _) =>
              newEnv
            case _ => env
          evalAll(tail, nextEnv, out2)
        catch
          case setup: CallCCSetup =>
            Continuations.replayCallCC(setup, head, tail, env, out)
          case ci: ContinuationInvoked =>
            Continuations.resumeContinuation(ci)

  /** Tie-the-knot: update named lambdas' closures so they can see all current bindings (enables mutual recursion at top
    * level).
    */
  private def patchClosures(env: Env): Env =
    val hasNamedLambda = env.bindings.exists { case (n, cell) =>
      cell(0) match
        case Value.LambdaVal(_, _, _, Some(ln), _) => ln == n
        case _                                     => false
    }
    if !hasNamedLambda then return env
    lazy val patched: Env = Env(
      env.bindings.map { case (n, cell) =>
        cell(0) match
          case Value.LambdaVal(ps, bd, _, ln @ Some(name), rp) if name == n =>
            n -> Array[Value](
              Value.LambdaVal(ps, bd, () => patched, ln, rp)
            )
          case _ => n -> cell
      },
      env.parent
    )
    patched

  /** Evaluate body exprs, returning Bounce for the last (tail position). */
  private[ming] def evalBodyTail(
    exprs: List[Value],
    env: Env,
    out: String,
    replayMode: Boolean = false
  ): EvalResult =
    exprs match
      case Nil => Done(Value.VoidVal, env, out)
      case last :: Nil =>
        if replayMode && Continuations.isCallCCForm(last) then
          val (v, e, o) =
            Continuations.evalBodyCallCC(last, Nil, env, out, replayMode)
          Done(v, e, o)
        else Bounce(last, env, out)
      case head :: tail =>
        val headResult = head match
          case _ if Continuations.isCallCCForm(head) =>
            Continuations.evalBodyCallCC(head, tail, env, out, replayMode)
          case _ =>
            try eval(head, env, out)
            catch
              case ci: ContinuationInvoked if ci.bodyLevel =>
                val cEnv = ci.envThunk()
                cEnv.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
                eval(ci.remaining.head, cEnv, ci.capturedOut)
        val (_, newEnv, out2) = headResult
        val nextEnv = head match
          case Value.PairVal(Value.Symbol("define", _), _, _)        => newEnv
          case Value.PairVal(Value.Symbol("define-syntax", _), _, _) => newEnv
          case _                                                     => env
        evalBodyTail(tail, nextEnv, out2, replayMode)

  /** Trampoline: evaluate expr, looping on Bounce until Done. */
  @tailrec
  private[ming] def eval(
    expr: Value,
    env: Env,
    out: String
  ): (Value, Env, String) =
    evalStep(expr, env, out) match
      case Done(v, e, o)       => (v, e, o)
      case Bounce(e2, env2, o) => eval(e2, env2, o)

  /** Single evaluation step — returns Bounce for tail positions. */
  private def evalStep(
    expr: Value,
    env: Env,
    out: String
  ): EvalResult =
    expr match
      case Value.IntVal(_)           => Done(expr, env, out)
      case Value.BoolVal(_)          => Done(expr, env, out)
      case Value.StringVal(_)        => Done(expr, env, out)
      case Value.CharVal(_)          => Done(expr, env, out)
      case Value.NilVal              => Done(expr, env, out)
      case Value.VoidVal             => Done(expr, env, out)
      case _: Value.MutableStringVal => Done(expr, env, out)
      case _: Value.LambdaVal        => Done(expr, env, out)
      case _: Value.ContinuationVal  => Done(expr, env, out)
      case _: Value.MacroVal         => Done(expr, env, out)
      case Value.Symbol(name, pos) =>
        Done(env.lookup(name, pos), env, out)
      case Value.PairVal(car, _, pos) =>
        val args = toList(expr).tail
        evalForm(car, args, env, pos, out)

  private def evalForm(
    op: Value,
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    op match
      case Value.Symbol("define", _) => Forms.evalDefine(args, env, pos, out)
      case Value.Symbol("if", _)     => Forms.evalIf(args, env, pos, out)
      case Value.Symbol("quote", _)  => Forms.evalQuote(args, env, out)
      case Value.Symbol("lambda", _) =>
        Done(Forms.makeLambda(args, env), env, out)
      case Value.Symbol("let", _)   => Forms.evalLet(args, env, out)
      case Value.Symbol("set!", _)  => Forms.evalSet(args, env, pos, out)
      case Value.Symbol("begin", _) => evalBodyTail(args, env, out)
      case Value.Symbol("cond", _)  => Forms.evalCond(args, env, out)
      case Value.Symbol("and", _)   => Forms.evalAnd(args, env, out)
      case Value.Symbol("or", _)    => Forms.evalOr(args, env, out)
      case Value.Symbol("not", _) =>
        val (v, out2) = Forms.evalNotInner(args, env, out)
        Done(v, env, out2)
      case Value.Symbol("display", _) =>
        Forms.evalDisplayForm(args, env, out, _.displayRepr)
      case Value.Symbol("write", _) =>
        Forms.evalDisplayForm(args, env, out, _.display)
      case Value.Symbol("newline", _) =>
        Done(Value.VoidVal, env, out + "\n")
      case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
        Continuations.handleCallCCForm(args, env, out)
      case Value.Symbol("define-syntax", _) =>
        Continuations.handleDefineSyntax(args, env, pos, out)
      case _ =>
        val (proc, _, out2) = eval(op, env, out)
        proc match
          case m: Value.MacroVal =>
            val (expanded, envBinds) = Macros.expand(m, op :: args)
            val expEnv = envBinds.foldLeft(env) { case (e, (k, v)) =>
              e.define(k, v)
            }
            Bounce(expanded, expEnv, out)
          case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
            Continuations.handleCallCCForm(args, env, out2)
          case _ =>
            val (evaledArgs, out3) = evalArgs(args, env, out2)
            applyProcTail(proc, evaledArgs, pos, out3)

  private def evalArgs(
    args: List[Value],
    env: Env,
    out: String
  ): (List[Value], String) =
    args.foldLeft((List.empty[Value], out)) { case ((acc, o), arg) =>
      val (v, _, o2) = eval(arg, env, o)
      (acc :+ v, o2)
    }

  /** Apply a procedure, returning Bounce for lambda bodies (TCO). */
  private[ming] def applyProcTail(
    proc: Value,
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    proc match
      case lam @ Value.LambdaVal(
            params,
            body,
            closureThunk,
            nameOpt,
            restParam
          ) =>
        val closure = closureThunk()
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val localEnv =
          closureWithSelf.extendVariadic(params, restParam, args, pos)
        evalBodyTail(body, localEnv, out)
      case Value.Symbol("apply", _) =>
        evalApply(args, pos, out)
      case cv: Value.ContinuationVal =>
        if args.length != 1 then throw EvalError.withPos("continuation requires 1 argument", pos)
        throw new ContinuationInvoked(
          cv.tag,
          args.head,
          out,
          cv.remaining,
          cv.envThunk,
          cv.capturedOut,
          cv.bodyLevel
        )
      case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
        if args.length != 1 then throw EvalError.withPos("call/cc requires 1 argument", pos)
        throw new CallCCSetup(args.head, out, pos)
      case Value.Symbol(name, _) =>
        Done(
          Builtins.applyBuiltin(name, args, pos),
          Env(Map.empty, None),
          out
        )
      case _ =>
        throw EvalError.withPos(
          s"not a procedure: ${proc.display}",
          pos
        )

  private def evalApply(
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    if args.length < 2 then throw EvalError.withPos("apply requires at least 2 arguments", pos)
    val proc       = args.head
    val prefixArgs = args.tail.init
    val lastArg    = args.last
    val listArgs   = toList(lastArg)
    applyProcTail(proc, prefixArgs ++ listArgs, pos, out)

  private[ming] def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private[ming] def toList(v: Value): List[Value] = v match
    case Value.NilVal           => Nil
    case Value.PairVal(h, t, _) => h :: toList(t)
    case other                  => List(other)
