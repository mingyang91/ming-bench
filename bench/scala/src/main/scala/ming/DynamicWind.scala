package ming

import Evaluator.{Bounce, Done, EvalResult}

/** Dynamic-wind implementation for L16. */
object DynamicWind:

  def evalDynamicWind(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case inExpr :: bodyExpr :: outExpr :: Nil =>
        val (inThunk, _, out1)   = Evaluator.eval(inExpr, env, out)
        val (bodyThunk, _, out2) = Evaluator.eval(bodyExpr, env, out1)
        val (outThunk, _, out3)  = Evaluator.eval(outExpr, env, out2)
        val (result, finalOut) =
          executeDynamicWind(inThunk, bodyThunk, outThunk, env, out3)
        Done(result, env, finalOut)
      case _ =>
        throw new EvalError("dynamic-wind requires exactly 3 arguments")

  private def executeDynamicWind(
    inThunk: Value,
    bodyThunk: Value,
    outThunk: Value,
    env: Env,
    out: String
  ): (Value, String) =
    val (_, _, out1) = callThunk(inThunk, out)
    val entry        = Value.PairVal(inThunk, outThunk)
    val prevStack    = lookupWindStack(env)
    val newStack     = Value.PairVal(entry, prevStack)
    setWindStack(env, newStack)
    try
      val (bodyVal, _, out2) = callThunk(bodyThunk, out1)
      setWindStack(env, prevStack)
      val (_, _, out3) = callThunk(outThunk, out2)
      (bodyVal, out3)
    catch
      case raised: SchemeRaised =>
        setWindStack(env, prevStack)
        val (_, _, outAfter) = callThunk(outThunk, raised.output)
        throw new SchemeRaised(raised.value, outAfter)
      case ci: ContinuationInvoked =>
        setWindStack(env, prevStack)
        val (_, _, outAfter) = callThunk(outThunk, ci.output)
        throw new ContinuationInvoked(
          ci.tag,
          ci.value,
          outAfter,
          ci.remaining,
          ci.envThunk,
          ci.capturedOut,
          ci.bodyLevel,
          ci.windEntries
        )
      case setup: CallCCSetup =>
        setWindStack(env, prevStack)
        throw setup

  private[ming] def callThunk(
    thunk: Value,
    out: String
  ): (Value, Env, String) =
    Evaluator.applyProcTail(thunk, Nil, None, out) match
      case Done(v, e, o)             => (v, e, o)
      case Bounce(e2, env2, o)       => Evaluator.eval(e2, env2, o)
      case gb: Evaluator.GuardBounce => ExceptionHandling.guardLoop(gb)

  private[ming] def lookupWindStack(env: Env): Value =
    try env.lookup("__wind_stack__")
    catch case _: EvalError => Value.NilVal

  private def setWindStack(env: Env, stack: Value): Unit =
    try env.set("__wind_stack__", stack, None)
    catch case _: EvalError => ()

  /** Convert wind stack Value to list of (in, out) pairs. */
  private[ming] def windStackToEntries(
    v: Value
  ): List[(Value, Value)] = v match
    case Value.PairVal(Value.PairVal(inT, outT, _), rest, _) =>
      (inT, outT) :: windStackToEntries(rest)
    case _ => Nil

  /** Wrap body in wind rewind frames (for continuation re-entry). */
  def withRewind(
    entries: List[(Value, Value)],
    env: Env,
    out: String,
    body: String => (Value, Env, String)
  ): (Value, Env, String) =
    entries match
      case Nil => body(out)
      case (inThunk, outThunk) :: rest =>
        val (_, _, out1) = callThunk(inThunk, out)
        val entry        = Value.PairVal(inThunk, outThunk)
        val prevStack    = lookupWindStack(env)
        val newStack     = Value.PairVal(entry, prevStack)
        setWindStack(env, newStack)
        try
          val (v, e, o) = withRewind(rest, env, out1, body)
          setWindStack(env, prevStack)
          val (_, _, o2) = callThunk(outThunk, o)
          (v, e, o2)
        catch
          case raised: SchemeRaised =>
            setWindStack(env, prevStack)
            val (_, _, outAfter) = callThunk(outThunk, raised.output)
            throw new SchemeRaised(raised.value, outAfter)
          case ci: ContinuationInvoked =>
            setWindStack(env, prevStack)
            val (_, _, outAfter) = callThunk(outThunk, ci.output)
            throw new ContinuationInvoked(
              ci.tag,
              ci.value,
              outAfter,
              ci.remaining,
              ci.envThunk,
              ci.capturedOut,
              ci.bodyLevel,
              ci.windEntries
            )
