package ming

import Display.display
import SchemeTypes.{errAt, pairToScalaList, Env, Pos, Value}
import CekSteps.{bodyToCek, posOf}

object Evaluator:

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
        CekSteps.stepLet(rest, env, p, k)

      case Expr.SList(Expr.Symbol("let*", _) :: rest, p) =>
        CekSteps.stepLetStar(rest, env, p, k)

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
      val contVal = Value.VContinuation(k)
      cekApply(proc, List(contVal), pos, env, k)

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

    case Value.VContinuation(savedK) =>
      if args.length != 1 then throw errAt(pos, "continuation requires 1 argument")
      CekState.ApplyK(args.head, savedK)

    case _ => throw errAt(pos, "not a procedure")

  // ── Backward-compatible recursive eval (for EvalCompound) ─────────
  private def evalExpr(expr: Expr, env: Env): Value =
    runCek(CekState.Eval(expr, env, Kont.Halt))

  private def evalBody(body: List[Expr], env: Env): Value =
    if body.isEmpty then Value.VVoid
    else runCek(bodyToCek(body, env, Kont.Halt))

  /** Apply a function to args (non-CEK path, used by builtins like map/for-each). */
  private[ming] def applyFunc(
    func: Value,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = func match
    case Value.VBuiltin("call/cc") | Value.VBuiltin("call-with-current-continuation") =>
      if args.length != 1 then throw errAt(pos, "call/cc requires 1 argument")
      val proc    = args.head
      val contVal = Value.VContinuation(Kont.Halt)
      applyFunc(proc, List(contVal), pos, env)
    case Value.VBuiltin("apply") =>
      if args.length < 2 then throw errAt(pos, "apply requires at least 2 arguments")
      val innerFunc = args.head
      val lastArg = args.last match
        case Value.VList(elems) => elems
        case Value.VPair(_)     => pairToScalaList(args.last, pos)
        case _                  => throw errAt(pos, "apply: last argument must be a list")
      val prefixArgs = args.slice(1, args.length - 1)
      applyFunc(innerFunc, prefixArgs ++ lastArg, pos, env)
    case Value.VBuiltin(name) => Builtins(name, args, pos, env)
    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      EvalTail.bindArgs(params, restParam, args, callEnv, pos)
      evalBody(body, callEnv)
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
          evalBody(body, callEnv)
        case None => throw errAt(pos, "wrong number of arguments")
    case Value.VContinuation(savedK) =>
      if args.length != 1 then throw errAt(pos, "continuation requires 1 argument")
      throw new ContinuationInvoke(args.head, savedK)
    case _ => throw errAt(pos, "not a procedure")

  // ── Public API ───────────────────────────────────────────────────────
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = defaultEnv()
    display(runCek(bodyToCek(exprs, env, Kont.Halt)))

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    val output = new StringBuilder
    val env    = defaultEnv(output)
    val result = display(runCek(bodyToCek(exprs, env, Kont.Halt)))
    (result, output.toString)
