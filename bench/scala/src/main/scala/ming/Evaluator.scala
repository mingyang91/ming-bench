package ming

import Display.display
import SchemeTypes.{errAt, pairToScalaList, Env, Pos, Value}
import CekSteps.{bodyToCek, posOf}

object Evaluator:

  // ── Dynamic-wind stack ────────────────────────────────────────────────
  private[ming] var windStack: List[WindEntry] = Nil

  // ── Exception handler stack ─────────────────────────────────────────
  private[ming] var exceptionHandlers: List[ExceptionHandler] = Nil

  private[ming] def commonTail(a: List[WindEntry], b: List[WindEntry]): List[WindEntry] =
    val aLen                = a.length
    val bLen                = b.length
    var aa: List[WindEntry] = a
    var bb: List[WindEntry] = b
    if aLen > bLen then for _ <- 0 until (aLen - bLen) do aa = aa.tail
    else for _ <- 0 until (bLen - aLen) do bb = bb.tail
    while !(aa eq bb) do
      aa = aa.tail
      bb = bb.tail
    aa

  // ── Exception handling ──────────────────────────────────────────────
  private def handleRaise(exnValue: Value): CekState =
    if exceptionHandlers.isEmpty then throw EvalError(s"unhandled exception: ${display(exnValue)}")
    val handler = exceptionHandlers.head
    exceptionHandlers = exceptionHandlers.tail
    handler match
      case ExceptionHandler.Guard(variable, clauses, env, guardK, savedWindStack) =>
        val guardTestK = Kont.GuardTest(variable, clauses, env, guardK)
        val common     = commonTail(windStack, savedWindStack)
        val toUnwind   = windStack.take(windStack.length - common.length)
        val toRewind   = savedWindStack.take(savedWindStack.length - common.length).reverse
        val ops        = toUnwind.map(e => (false, e)) ++ toRewind.map(e => (true, e))
        if ops.isEmpty then CekState.ApplyK(exnValue, guardTestK)
        else
          val dummyPos = Pos(0, 0)
          CekState.ApplyK(
            Value.VVoid,
            Kont.DynWindTransition(ops, exnValue, guardTestK, env, dummyPos)
          )
      case ExceptionHandler.Proc(handlerProc, env, _) =>
        val dummyPos = Pos(0, 0)
        cekApply(handlerProc, List(exnValue), dummyPos, env, Kont.RaiseReturn(Kont.Halt))

  // ── Environment ──────────────────────────────────────────────────────
  private def defaultEnv(
    output: StringBuilder = new StringBuilder
  ): Env =
    val env =
      Env(scala.collection.mutable.Map.empty, None, output)
    for name <- BuiltinNames.all do env.define(name, Value.VBuiltin(name))
    env

  // ── CEK main loop ──────────────────────────────────────────────────
  private def runCek(state0: CekState): Value =
    var state = state0
    while true do
      state =
        try
          state match
            case CekState.ApplyK(v, Kont.Halt) => return v
            case CekState.Eval(expr, env, k)   => evalStep(expr, env, k)
            case CekState.ApplyK(v, k)         => CekSteps.kontStep(v, k)
        catch
          case ci: ContinuationInvoke =>
            CekState.ApplyK(ci.value, ci.kont)
          case sr: SchemeRaise =>
            handleRaise(sr.value)
    throw EvalError("unreachable")

  // ── Eval step ──────────────────────────────────────────────────────
  private def evalStep(expr: Expr, env: Env, k: Kont): CekState =
    expr match
      case Expr.Num(n, _)       => CekState.ApplyK(Value.VNum(n), k)
      case Expr.Flt(d, _)       => CekState.ApplyK(Value.VFloat(d), k)
      case Expr.Rat(n, d, _)    => CekState.ApplyK(Value.VRational(n, d), k)
      case Expr.Bool(b, _)      => CekState.ApplyK(Value.VBool(b), k)
      case Expr.Str(s, _)       => CekState.ApplyK(Value.VStr(s.toCharArray, mutable = false), k)
      case Expr.Chr(c, _)       => CekState.ApplyK(Value.VChar(c), k)
      case Expr.Symbol(name, p) => CekState.ApplyK(env.lookup(name, p), k)
      case Expr.SList(Nil, p)   => throw errAt(p, "empty application")

      // ── Special forms ──────────────────────────────────────────────
      case Expr.SList(Expr.Symbol("quote", _) :: arg :: Nil, _) =>
        CekState.ApplyK(EvalForms.quoteToValue(arg), k)

      case Expr.SList(Expr.Symbol("define", _) :: rest, p) =>
        CekSteps.stepDefine(rest, env, p, k)

      case Expr.SList(Expr.Symbol("if", _) :: rest, p) =>
        CekSteps.stepIf(rest, env, p, k)

      case Expr.SList(Expr.Symbol("lambda", _) :: rest, p) =>
        CekState.ApplyK(CekSteps.makeLambda(rest, env, p), k)

      case Expr.SList(Expr.Symbol("and", _) :: args, _) =>
        CekSteps.stepAnd(args, env, k)

      case Expr.SList(Expr.Symbol("or", _) :: args, _) =>
        CekSteps.stepOr(args, env, k)

      case Expr.SList(Expr.Symbol("let", _) :: rest, p) =>
        CekLetSteps.stepLet(rest, env, p, k)

      case Expr.SList(Expr.Symbol("let*", _) :: rest, p) =>
        CekLetSteps.stepLetStar(rest, env, p, k)

      case Expr.SList(Expr.Symbol("begin", _) :: body, _) =>
        bodyToCek(body, env, k)

      case Expr.SList(Expr.Symbol("cond", _) :: clauses, _) =>
        CekSteps.stepCond(clauses, env, k)

      case Expr.SList(Expr.Symbol("set!", _) :: rest, p) =>
        CekSteps.stepSet(rest, env, p, k)

      case Expr.SList(Expr.Symbol("define-syntax", _) :: rest, p) =>
        CekState.ApplyK(EvalForms.evalDefineSyntax(rest, env, p), k)

      case Expr.SList(Expr.Symbol("case-lambda", _) :: clauses, p) =>
        CekState.ApplyK(EvalForms.evalCaseLambda(clauses, env, p), k)

      case Expr.SList(Expr.Symbol("define-record-type", _) :: rest, p) =>
        CekState.ApplyK(EvalForms.evalDefineRecordType(rest, env, p), k)

      case Expr.SList(Expr.Symbol("letrec", _) :: rest, p) =>
        CekState.ApplyK(EvalCompound.evalLetrec(rest, env, p, evalExpr, evalBody), k)

      case Expr.SList(Expr.Symbol("letrec*", _) :: rest, p) =>
        CekState.ApplyK(EvalCompound.evalLetrecStar(rest, env, p, evalExpr, evalBody), k)

      case Expr.SList(Expr.Symbol("case", _) :: rest, p) =>
        CekState.ApplyK(EvalCompound.evalCase(rest, env, p, evalExpr, evalBody, posOf), k)

      case Expr.SList(Expr.Symbol("do", _) :: rest, p) =>
        CekState.ApplyK(EvalCompound.evalDo(rest, env, p, evalExpr, evalBody, posOf), k)

      case Expr.SList(Expr.Symbol("guard", _) :: Expr.SList(Expr.Symbol(variable, _) :: clauses, _) :: body, _) =>
        exceptionHandlers = ExceptionHandler.Guard(variable, clauses, env, k, windStack) :: exceptionHandlers
        bodyToCek(body, env, Kont.PopHandler(k))

      // ── Application ────────────────────────────────────────────────
      case Expr.SList(head :: args, p) =>
        CekSteps.stepApp(head, args, env, p, k)

  // ── Function application (CEK) ────────────────────────────────────
  private[ming] def cekApply(
    func: Value,
    args: List[Value],
    pos: Pos,
    env: Env,
    k: Kont
  ): CekState = func match
    case Value.VBuiltin("call/cc") | Value.VBuiltin("call-with-current-continuation") =>
      if args.length != 1 then throw errAt(pos, "call/cc requires 1 argument")
      val proc    = args.head
      val contVal = Value.VContinuation(k, windStack)
      cekApply(proc, List(contVal), pos, env, k)

    case Value.VBuiltin("raise") =>
      if args.length != 1 then throw errAt(pos, "raise requires 1 argument")
      throw new SchemeRaise(args.head)

    case Value.VBuiltin("with-exception-handler") =>
      if args.length != 2 then throw errAt(pos, "with-exception-handler requires 2 arguments")
      val handler = args(0)
      val thunk   = args(1)
      exceptionHandlers = ExceptionHandler.Proc(handler, env, windStack) :: exceptionHandlers
      cekApply(thunk, Nil, pos, env, Kont.PopHandler(k))

    case Value.VBuiltin("dynamic-wind") =>
      if args.length != 3 then throw errAt(pos, "dynamic-wind requires 3 arguments")
      val inThunk   = args(0)
      val bodyThunk = args(1)
      val outThunk  = args(2)
      val entry     = new WindEntry(inThunk, outThunk)
      val afterIn   = Kont.DynWindAfterIn(bodyThunk, entry, env, pos, k)
      cekApply(inThunk, Nil, pos, env, afterIn)

    case Value.VBuiltin("values") =>
      args match
        case single :: Nil => CekState.ApplyK(single, k)
        case _             => CekState.ApplyK(Value.VValues(args), k)

    case Value.VBuiltin("call-with-values") =>
      if args.length != 2 then throw errAt(pos, "call-with-values requires 2 arguments")
      val producer = args(0)
      val consumer = args(1)
      cekApply(producer, Nil, pos, env, Kont.CallWithValues(consumer, env, pos, k))

    case Value.VBuiltin("apply") =>
      if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
      val innerFunc = args.head
      val lastArg = args.last match
        case Value.VList(elems) => elems
        case Value.VPair(_)     => pairToScalaList(args.last, pos)
        case _                  => throw errAt(pos, "apply: last argument must be a list")
      val prefixArgs = args.slice(1, args.length - 1)
      cekApply(innerFunc, prefixArgs ++ lastArg, pos, env, k)

    case Value.VBuiltin(name) =>
      try
        val result = Builtins(name, args, pos, env)
        CekState.ApplyK(result, k)
      catch
        case ci: ContinuationInvoke =>
          CekState.ApplyK(ci.value, ci.kont)

    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      EvalTail.bindArgs(params, restParam, args, callEnv, pos)
      bodyToCek(body, callEnv, k)

    case Value.VCaseLambda(clauses) =>
      val matched = clauses.find { case (params, restParam, _, _) =>
        restParam match
          case None    => args.length == params.length
          case Some(_) => args.length >= params.length
      }
      matched match
        case Some((params, restParam, body, closure)) =>
          val callEnv = closure.child()
          EvalTail.bindArgs(params, restParam, args, callEnv, pos)
          bodyToCek(body, callEnv, k)
        case None => throw errAt(pos, "wrong number of arguments")

    case Value.VContinuation(savedK, savedWindStack) =>
      if args.length != 1 then throw errAt(pos, "continuation requires 1 argument")
      val common   = commonTail(windStack, savedWindStack)
      val toUnwind = windStack.take(windStack.length - common.length)
      val toRewind = savedWindStack.take(savedWindStack.length - common.length).reverse
      val ops: List[(Boolean, WindEntry)] =
        toUnwind.map(e => (false, e)) ++ toRewind.map(e => (true, e))
      if ops.isEmpty then CekState.ApplyK(args.head, savedK)
      else
        CekState.ApplyK(
          Value.VVoid,
          Kont.DynWindTransition(ops, args.head, savedK, env, pos)
        )

    case _ => throw errAt(pos, "not a procedure")

  // ── Backward-compatible recursive eval (for EvalCompound) ─────────
  private def evalExpr(expr: Expr, env: Env): Value =
    runCek(CekState.Eval(expr, env, Kont.Halt))

  private[ming] def runCekInternal(state: CekState): Value = runCek(state)

  private def evalBody(body: List[Expr], env: Env): Value =
    if body.isEmpty then Value.VVoid
    else runCek(bodyToCek(body, env, Kont.Halt))

  // ── Public API ───────────────────────────────────────────────────────
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    windStack = Nil
    exceptionHandlers = Nil
    val env = defaultEnv()
    display(runCek(bodyToCek(exprs, env, Kont.Halt)))

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    windStack = Nil
    exceptionHandlers = Nil
    val output = new StringBuilder
    val env    = defaultEnv(output)
    val result = display(runCek(bodyToCek(exprs, env, Kont.Halt)))
    (result, output.toString)
