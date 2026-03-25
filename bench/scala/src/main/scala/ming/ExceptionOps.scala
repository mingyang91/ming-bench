package ming

/** Exception handling: raise, guard clause evaluation. */
object ExceptionOps:

  /** Handle a raised exception by dispatching to the top handler. */
  def handleRaise(
    value: SchemeVal,
    raiserK: Cont,
    performApply: (SchemeVal, List[SchemeVal], Cont) => Evaluator.State
  ): Evaluator.State =
    val handlers = Evaluator.handlerStack.get()
    if handlers.isEmpty then throw new EvalError(s"unhandled exception: ${value.display}")
    val handler = handlers.head
    Evaluator.handlerStack.set(handlers.tail)
    handler match
      case ExceptionHandler.Proc(handlerProc, _) =>
        performApply(handlerProc, List(value), Cont.RaiseReturnErrorK)
      case ExceptionHandler.Guard(exnVar, clauses, env, guardK, savedWinds) =>
        val guardEnv = Env(Some(env))
        guardEnv.define(exnVar, value)
        val currentWinds = Evaluator.windStack.get()
        val actions      = DynWind.computeWindActions(currentWinds, savedWinds)
        if actions.isEmpty then
          Evaluator.windStack.set(savedWinds)
          evalGuardClauses(clauses, value, guardEnv, guardK, performApply)
        else
          DynWind.startWindActions(
            actions,
            SchemeVal.SVoid,
            Cont.GuardAfterWindK(clauses, value, guardEnv, guardK),
            savedWinds
          )(performApply)

  /** Evaluate guard clauses against a raised exception value. */
  def evalGuardClauses(
    clauses: List[SchemeVal],
    exnValue: SchemeVal,
    env: Env,
    guardK: Cont,
    performApply: (SchemeVal, List[SchemeVal], Cont) => Evaluator.State
  ): Evaluator.State =
    clauses match
      case Nil =>
        handleRaise(exnValue, guardK, performApply)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
            Evaluator.evalBodyCek(body, env, guardK)
          case SchemeVal.SList(test :: body) =>
            Evaluator.State.Ev(test, env, Cont.GuardTestK(body, rest, exnValue, env, guardK))
          case _ => throw new EvalError("guard: bad clause")
