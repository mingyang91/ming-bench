package ming

/** Extended continuation stepping — dynamic-wind, exceptions, values, let bindings. */
object KontOps:

  import Evaluator.{evalBodyCek, handlerStack, isTruthy, windStack, State}

  def step(v: SchemeVal, k: Cont): State = k match
    // dynamic-wind normal flow
    case Cont.DynWindAfterInK(bodyThunk, entry, k2) =>
      windStack.set(entry :: windStack.get())
      Apply.performApply(bodyThunk, Nil, Cont.DynWindAfterBodyK(entry, k2))
    case Cont.DynWindAfterBodyK(entry, k2) =>
      val ws = windStack.get()
      if ws.nonEmpty && (ws.head eq entry) then windStack.set(ws.tail)
      Apply.performApply(entry.outThunk, Nil, Cont.DynWindAfterOutK(v, k2))
    case Cont.DynWindAfterOutK(bodyValue, k2) =>
      State.Ko(bodyValue, k2)
    // continuation wind/unwind steps
    case Cont.WindContinueK(remaining, finalValue, targetK, targetWinds) =>
      DynWind.startWindActions(remaining, finalValue, targetK, targetWinds)(Apply.performApply)
    case Cont.WindPushK(entry, remaining, finalValue, targetK, targetWinds) =>
      windStack.set(entry :: windStack.get())
      DynWind.startWindActions(remaining, finalValue, targetK, targetWinds)(Apply.performApply)
    // exception handling (L20)
    case Cont.WithHandlerK(k2) =>
      val hs = handlerStack.get()
      if hs.nonEmpty then handlerStack.set(hs.tail)
      State.Ko(v, k2)
    case Cont.RaiseReturnErrorK =>
      throw new EvalError("handler returned from non-continuable exception")
    case Cont.GuardAfterWindK(clauses, exnValue, env, guardK) =>
      ExceptionOps.evalGuardClauses(clauses, exnValue, env, guardK, Apply.performApply)
    case Cont.GuardTestK(body, remaining, exnValue, env, guardK) =>
      if isTruthy(v) then
        if body.isEmpty then State.Ko(v, guardK)
        else evalBodyCek(body, env, guardK)
      else ExceptionOps.evalGuardClauses(remaining, exnValue, env, guardK, Apply.performApply)
    // L21 — call-with-values
    case Cont.CallWithValuesK(consumer, k2) =>
      v match
        case SchemeVal.SValues(vals) => Apply.performApply(consumer, vals, k2)
        case single                  => Apply.performApply(consumer, List(single), k2)
    // case expression — v is the key, match against clauses
    case Cont.CaseK(clauses, env, k2) =>
      evalCaseClauses(v, clauses, env, k2)
    // CPS let binding evaluation
    case Cont.LetEvalK(currentName, bound, remaining, body, outerEnv, k2) =>
      val newBound = (currentName, v) :: bound
      remaining match
        case Nil =>
          val letEnv = Env(Some(outerEnv))
          newBound.reverse.foreach((n, value) => letEnv.define(n, value))
          evalBodyCek(body, letEnv, k2)
        case (nextName, nextExpr) :: rest =>
          State.Ev(nextExpr, outerEnv, Cont.LetEvalK(nextName, newBound, rest, body, outerEnv, k2))
    // CPS named let binding evaluation
    case Cont.NamedLetEvalK(loopName, paramNames, currentName, bound, remaining, body, outerEnv, k2) =>
      val newBound = (currentName, v) :: bound
      remaining match
        case Nil =>
          val letEnv = Env(Some(outerEnv))
          letEnv.define(loopName, SchemeVal.SLambda(paramNames, None, body, letEnv))
          newBound.reverse.foreach((n, value) => letEnv.define(n, value))
          evalBodyCek(body, letEnv, k2)
        case (nextName, nextExpr) :: rest =>
          State.Ev(
            nextExpr,
            outerEnv,
            Cont.NamedLetEvalK(loopName, paramNames, nextName, newBound, rest, body, outerEnv, k2)
          )

  @scala.annotation.tailrec
  private def evalCaseClauses(key: SchemeVal, clauses: List[SchemeVal], env: Env, k: Cont): State =
    clauses match
      case Nil => State.Ko(SchemeVal.SVoid, k)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
            evalBodyCek(body, env, k)
          case SchemeVal.SList(SchemeVal.SList(datums) :: body) =>
            if datums.exists(d => Builtins.schemeEqv(key, d)) then
              if body.isEmpty then State.Ko(SchemeVal.SVoid, k)
              else evalBodyCek(body, env, k)
            else evalCaseClauses(key, rest, env, k)
          case _ => throw new EvalError("case: bad clause")
