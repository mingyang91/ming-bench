package ming

import SchemeTypes.{errAt, isTruthy, Env, Pos, Value}

/** CEK machine state. */
enum CekState:
  case Eval(expr: Expr, env: Env, k: Kont)
  case ApplyK(value: Value, k: Kont)

/** Step helpers for the CEK machine — special forms and continuation dispatch. */
object CekSteps:

  // ── Position helpers ────────────────────────────────────────────────
  private[ming] def posOf(expr: Expr): Pos = expr match
    case Expr.Num(_, p)    => p
    case Expr.Flt(_, p)    => p
    case Expr.Rat(_, _, p) => p
    case Expr.Bool(_, p)   => p
    case Expr.Str(_, p)    => p
    case Expr.Chr(_, p)    => p
    case Expr.Symbol(_, p) => p
    case Expr.SList(_, p)  => p

  // ── Body → CEK state ──────────────────────────────────────────────
  private[ming] def bodyToCek(body: List[Expr], env: Env, k: Kont): CekState =
    body match
      case Nil          => CekState.ApplyK(Value.VVoid, k)
      case last :: Nil  => CekState.Eval(last, env, k)
      case head :: rest => CekState.Eval(head, env, Kont.Seq(rest, env, k))

  // ── Continuation step ──────────────────────────────────────────────
  private[ming] def kontStep(v: Value, k: Kont): CekState = k match
    case Kont.Halt =>
      throw EvalError("unreachable: kontStep on Halt")

    case Kont.Seq(remaining, env, next) =>
      remaining match
        case Nil          => CekState.ApplyK(v, next)
        case last :: Nil  => CekState.Eval(last, env, next)
        case head :: rest => CekState.Eval(head, env, Kont.Seq(rest, env, next))

    case Kont.Define(name, env, next) =>
      env.define(name, v)
      CekState.ApplyK(Value.VVoid, next)

    case Kont.Set(name, env, pos, next) =>
      env.set(name, v, pos)
      CekState.ApplyK(Value.VVoid, next)

    case Kont.If(thenExpr, elseExpr, env, next) =>
      if isTruthy(v) then CekState.Eval(thenExpr, env, next)
      else
        elseExpr match
          case Some(e) => CekState.Eval(e, env, next)
          case None    => CekState.ApplyK(Value.VVoid, next)

    case Kont.And(remaining, env, next) =>
      v match
        case Value.VBool(false) => CekState.ApplyK(Value.VBool(false), next)
        case _ =>
          remaining match
            case Nil          => CekState.ApplyK(v, next)
            case last :: Nil  => CekState.Eval(last, env, next)
            case head :: rest => CekState.Eval(head, env, Kont.And(rest, env, next))

    case Kont.Or(remaining, env, next) =>
      if isTruthy(v) then CekState.ApplyK(v, next)
      else
        remaining match
          case Nil          => CekState.ApplyK(v, next)
          case last :: Nil  => CekState.Eval(last, env, next)
          case head :: rest => CekState.Eval(head, env, Kont.Or(rest, env, next))

    case Kont.LetBind(name, remaining, letEnv, initEnv, body, next) =>
      letEnv.define(name, v)
      remaining match
        case Nil =>
          bodyToCek(body, letEnv, next)
        case (nextName, nextExpr) :: rest =>
          CekState.Eval(nextExpr, initEnv, Kont.LetBind(nextName, rest, letEnv, initEnv, body, next))

    case Kont.LetStarBind(name, remaining, letEnv, body, next) =>
      letEnv.define(name, v)
      remaining match
        case Nil =>
          bodyToCek(body, letEnv, next)
        case (nextName, nextExpr) :: rest =>
          CekState.Eval(nextExpr, letEnv, Kont.LetStarBind(nextName, rest, letEnv, body, next))

    case Kont.NamedLetArgs(params, doneVals, remainingExprs, initEnv, loopEnv, body, next) =>
      val newDone = doneVals :+ v
      remainingExprs match
        case Nil =>
          val callEnv = loopEnv.child()
          params.zip(newDone).foreach((p, a) => callEnv.define(p, a))
          bodyToCek(body, callEnv, next)
        case nextExpr :: rest =>
          CekState.Eval(
            nextExpr,
            initEnv,
            Kont.NamedLetArgs(params, newDone, rest, initEnv, loopEnv, body, next)
          )

    case Kont.CondTest(body, remaining, env, next) =>
      if isTruthy(v) then
        if body.nonEmpty then bodyToCek(body, env, next)
        else CekState.ApplyK(v, next)
      else stepCond(remaining, env, next)

    case k: Kont.DynWindAfterIn    => stepDynWind(v, k)
    case k: Kont.DynWindAfterBody  => stepDynWind(v, k)
    case k: Kont.DynWindAfterOut   => stepDynWind(v, k)
    case k: Kont.DynWindTransition => stepDynWind(v, k)
    case k: Kont.AppHead           => stepAppKont(v, k)
    case k: Kont.AppArg            => stepAppKont(v, k)

  // ── Dynamic-wind continuation helpers ─────────────────────────────
  private def stepDynWind(v: Value, k: Kont): CekState = k match
    case Kont.DynWindAfterIn(bodyThunk, entry, env, pos, next) =>
      Evaluator.windStack = entry :: Evaluator.windStack
      val afterBody = Kont.DynWindAfterBody(entry, env, pos, next)
      Evaluator.cekApply(bodyThunk, Nil, pos, env, afterBody)

    case Kont.DynWindAfterBody(entry, env, pos, next) =>
      Evaluator.windStack = Evaluator.windStack.tail
      val afterOut = Kont.DynWindAfterOut(v, next)
      Evaluator.cekApply(entry.outThunk, Nil, pos, env, afterOut)

    case Kont.DynWindAfterOut(bodyResult, next) =>
      CekState.ApplyK(bodyResult, next)

    case Kont.DynWindTransition(ops, value, savedK, env, pos) =>
      ops match
        case Nil =>
          CekState.ApplyK(value, savedK)
        case (isIn, entry) :: rest =>
          val nextK = Kont.DynWindTransition(rest, value, savedK, env, pos)
          if isIn then
            Evaluator.windStack = entry :: Evaluator.windStack
            Evaluator.cekApply(entry.inThunk, Nil, pos, env, nextK)
          else
            Evaluator.windStack = Evaluator.windStack.tail
            Evaluator.cekApply(entry.outThunk, Nil, pos, env, nextK)

    case _ => throw EvalError("unreachable: stepDynWind")

  // ── Application continuation helpers ─────────────────────────────
  private def stepAppKont(v: Value, k: Kont): CekState = k match
    case Kont.AppHead(argExprs, env, pos, next) =>
      argExprs match
        case Nil =>
          Evaluator.cekApply(v, Nil, pos, env, next)
        case _ =>
          val rev = argExprs.reverse
          CekState.Eval(rev.head, env, Kont.AppArg(v, Nil, rev.tail, env, pos, next))

    case Kont.AppArg(func, done, remaining, env, pos, next) =>
      val newDone = v :: done
      remaining match
        case Nil =>
          Evaluator.cekApply(func, newDone, pos, env, next)
        case nextExpr :: rest =>
          CekState.Eval(nextExpr, env, Kont.AppArg(func, newDone, rest, env, pos, next))

    case _ => throw EvalError("unreachable: stepAppKont")

  // ── Special form step helpers ──────────────────────────────────────

  private[ming] def stepDefine(rest: List[Expr], env: Env, pos: Pos, k: Kont): CekState =
    rest match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        CekState.Eval(valueExpr, env, Kont.Define(name, env, k))
      case Expr.SList(Expr.Symbol(name, _) :: params, _) :: body =>
        val (paramNames, restParam) = EvalForms.parseParams(params, pos)
        env.define(name, Value.VLambda(paramNames, restParam, body, env))
        CekState.ApplyK(Value.VVoid, k)
      case _ => throw errAt(pos, "invalid define")

  private[ming] def stepIf(rest: List[Expr], env: Env, pos: Pos, k: Kont): CekState =
    rest match
      case cond :: thenExpr :: elseExpr :: Nil =>
        CekState.Eval(cond, env, Kont.If(thenExpr, Some(elseExpr), env, k))
      case cond :: thenExpr :: Nil =>
        CekState.Eval(cond, env, Kont.If(thenExpr, None, env, k))
      case _ => throw errAt(pos, "invalid if")

  private[ming] def makeLambda(rest: List[Expr], env: Env, pos: Pos): Value =
    rest match
      case Expr.SList(params, _) :: body =>
        val (paramNames, restParam) = EvalForms.parseParams(params, pos)
        Value.VLambda(paramNames, restParam, body, env)
      case Expr.Symbol(name, _) :: body =>
        Value.VLambda(Nil, Some(name), body, env)
      case _ => throw errAt(pos, "invalid lambda")

  private[ming] def stepAnd(args: List[Expr], env: Env, k: Kont): CekState =
    args match
      case Nil          => CekState.ApplyK(Value.VBool(true), k)
      case last :: Nil  => CekState.Eval(last, env, k)
      case head :: rest => CekState.Eval(head, env, Kont.And(rest, env, k))

  private[ming] def stepOr(args: List[Expr], env: Env, k: Kont): CekState =
    args match
      case Nil          => CekState.ApplyK(Value.VBool(false), k)
      case last :: Nil  => CekState.Eval(last, env, k)
      case head :: rest => CekState.Eval(head, env, Kont.Or(rest, env, k))

  private[ming] def stepSet(rest: List[Expr], env: Env, pos: Pos, k: Kont): CekState =
    rest match
      case Expr.Symbol(name, p) :: valueExpr :: Nil =>
        CekState.Eval(valueExpr, env, Kont.Set(name, env, p, k))
      case _ => throw errAt(pos, "invalid set!")

  private[ming] def stepCond(clauses: List[Expr], env: Env, k: Kont): CekState =
    clauses match
      case Nil => CekState.ApplyK(Value.VVoid, k)
      case Expr.SList(Expr.Symbol("else", _) :: body, _) :: _ =>
        bodyToCek(body, env, k)
      case Expr.SList(test :: body, _) :: rest =>
        CekState.Eval(test, env, Kont.CondTest(body, rest, env, k))
      case e :: _ => throw errAt(posOf(e), "invalid cond")

  private[ming] def stepApp(
    head: Expr,
    args: List[Expr],
    env: Env,
    pos: Pos,
    k: Kont
  ): CekState =
    head match
      case Expr.Symbol(name, _) =>
        env.lookupOpt(name) match
          case Some(m: Value.VMacro) =>
            val (expanded, injections) =
              MacroExpander.expand(m, Expr.SList(head :: args, pos), pos)
            val macroEnv = env.child()
            injections.foreach((key, v) => macroEnv.define(key, v))
            CekState.Eval(expanded, macroEnv, k)
          case _ =>
            CekState.Eval(head, env, Kont.AppHead(args, env, pos, k))
      case _ =>
        CekState.Eval(head, env, Kont.AppHead(args, env, pos, k))
