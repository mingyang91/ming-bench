package ming

import SchemeValue.*

/** Guard frame for trampoline-based guard handling. */
private[ming] case class GuardFrame(varName: String, clauses: List[SchemeValue], env: Environment)

/** Dynamic-wind, raise, with-exception-handler, and guard operations. */
object WindException:

  /** Transition wind stack from current to target, calling out/in thunks. */
  def doWindTransition(target: List[(SchemeValue, SchemeValue)]): Unit =
    val current = ContinuationManager.windStack
    if current eq target then return
    val currentLen = current.length
    val targetLen  = target.length
    // Find common tail by reference equality
    var c = current
    var t = target
    if currentLen > targetLen then
      var i = 0
      while i < currentLen - targetLen do
        c = c.tail; i += 1
    else
      var i = 0
      while i < targetLen - currentLen do
        t = t.tail; i += 1
    while !(c eq t) do
      c = c.tail; t = t.tail
    val commonLen = c.length
    // Unwind: call out-thunks from innermost to outermost
    for _ <- 0 until (currentLen - commonLen) do
      val frame = ContinuationManager.windStack.head
      ContinuationManager.windStack = ContinuationManager.windStack.tail
      Interpreter.applyProc(frame._2, Nil)
    // Rewind: call in-thunks from outermost to innermost
    val rewindFrames = target.take(targetLen - commonLen).reverse
    for frame <- rewindFrames do
      Interpreter.applyProc(frame._1, Nil)
      ContinuationManager.windStack = frame :: ContinuationManager.windStack

  /** Implement dynamic-wind: in-thunk, body-thunk, out-thunk. */
  def dynamicWindOp(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("dynamic-wind: requires 3 arguments")
    val inThunk   = args(0)
    val bodyThunk = args(1)
    val outThunk  = args(2)
    Interpreter.applyProc(inThunk, Nil)
    val frame = (inThunk, outThunk)
    ContinuationManager.windStack = frame :: ContinuationManager.windStack
    try
      val result = Interpreter.applyProc(bodyThunk, Nil)
      ContinuationManager.windStack = ContinuationManager.windStack.tail
      Interpreter.applyProc(outThunk, Nil)
      result
    catch
      case e: SchemeRaise =>
        ContinuationManager.windStack = ContinuationManager.windStack.tail
        Interpreter.applyProc(outThunk, Nil)
        throw e

  /** Implement raise: throw a Scheme-level exception. */
  def raiseOp(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("raise: requires 1 argument")
    throw new SchemeRaise(args.head)

  /** Implement with-exception-handler: install handler and run thunk. */
  def withExceptionHandlerOp(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("with-exception-handler: requires 2 arguments")
    val handler = args(0)
    val thunk   = args(1)
    try Interpreter.applyProc(thunk, Nil)
    catch
      case e: SchemeRaise =>
        Interpreter.applyProc(handler, List(e.value))

  /** Evaluate a guard form: (guard (var clause ...) body ...). */
  private[ming] def evalGuard(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    args match
      case ListVal(SymbolVal(varName, _) :: clauses, _) :: body if body.nonEmpty =>
        try
          if body.length == 1 then Interpreter.eval(body.head, env)
          else
            Interpreter.evalBodyInit(body, env)
            Interpreter.eval(body.last, env)
        catch
          case e: SchemeRaise =>
            val guardEnv = env.child()
            guardEnv.define(varName, e.value)
            evalGuardClauses(clauses, guardEnv, e)
      case _ => throw new EvalError("guard: bad syntax")

  /** Parse a guard form's arguments, returning (varName, clauses, body). */
  private[ming] def parseGuard(
    args: List[SchemeValue]
  ): (String, List[SchemeValue], List[SchemeValue]) =
    args match
      case ListVal(SymbolVal(varName, _) :: clauses, _) :: body if body.nonEmpty =>
        (varName, clauses, body)
      case _ => throw new EvalError("guard: bad syntax")

  private[ming] def evalGuardClauses(
    clauses: List[SchemeValue],
    env: Environment,
    original: SchemeRaise
  ): SchemeValue =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case ListVal(SymbolVal("else", _) :: body, _) =>
          if body.isEmpty then throw new EvalError("guard: else with no body")
          Interpreter.evalBodyInit(body, env)
          return Interpreter.eval(body.last, env)
        case ListVal(test :: body, _) =>
          val result = Interpreter.eval(test, env)
          if result.isTruthy then
            if body.isEmpty then return result
            Interpreter.evalBodyInit(body, env)
            return Interpreter.eval(body.last, env)
          remaining = remaining.tail
        case _ => throw new EvalError("guard: bad clause")
    // No clause matched, re-raise
    throw original

  /** Try guard frames in order; return remaining frames and result, or re-raise. */
  private[ming] def tryGuardFrames(
    frames: List[GuardFrame],
    initial: SchemeRaise
  ): (List[GuardFrame], SchemeValue) =
    var remaining = frames
    var ex        = initial
    while remaining.nonEmpty do
      val frame = remaining.head
      remaining = remaining.tail
      val guardEnv = frame.env.child()
      guardEnv.define(frame.varName, ex.value)
      try return (remaining, evalGuardClauses(frame.clauses, guardEnv, ex))
      catch case reRaise: SchemeRaise => ex = reRaise
    throw ex
