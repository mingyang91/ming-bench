package ming

import DynamicWind.Winder

/** Guard special form — exception handling with cond-like clauses. */
object Guard:

  /** Evaluate guard special form. */
  def evalGuardK(
    args: List[SchemeVal],
    env: Env,
    k: Evaluator.Cont
  ): Evaluator.Bounce =
    if args.size < 2 then throw new EvalError("guard: expected at least 2 arguments")
    val (varName, clauses) = args.head match
      case SchemeVal.SList(items) if items.nonEmpty =>
        items.head match
          case SchemeVal.Symbol(name) => (name, items.tail)
          case _                      => throw new EvalError("guard: expected variable name")
      case _ => throw new EvalError("guard: malformed")
    val body          = args.tail
    val guardK        = k
    val guardWinders  = Evaluator.winders
    val guardEnv      = env
    val savedHandlers = Evaluator.exceptionHandlers

    val handler: Evaluator.ExceptionHandler = exnVal =>
      val currentWinders = Evaluator.winders
      DynamicWind.doWindK(
        currentWinders,
        guardWinders,
        () =>
          val clauseEnv =
            new Env(scala.collection.mutable.Map(varName -> exnVal), Some(guardEnv))
          evalGuardClausesK(clauses, clauseEnv, guardK, exnVal)
      )

    Evaluator.exceptionHandlers = handler :: Evaluator.exceptionHandlers
    Evaluator.evalBodyK(
      body,
      env,
      result =>
        Evaluator.exceptionHandlers = savedHandlers
        guardK(result)
    )

  /** Evaluate guard cond-like clauses. */
  private def evalGuardClausesK(
    clauses: List[SchemeVal],
    env: Env,
    k: Evaluator.Cont,
    exnVal: SchemeVal
  ): Evaluator.Bounce =
    clauses match
      case Nil =>
        if Evaluator.exceptionHandlers.isEmpty then
          throw new EvalError(s"unhandled exception: ${SchemeVal.display(exnVal)}")
        val handler = Evaluator.exceptionHandlers.head
        Evaluator.exceptionHandlers = Evaluator.exceptionHandlers.tail
        handler(exnVal)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(items) if items.nonEmpty =>
            items.head match
              case SchemeVal.Symbol("else") =>
                Evaluator.evalBodyK(items.tail, env, k)
              case test =>
                Evaluator.More(() =>
                  Evaluator.evalK(
                    test,
                    env,
                    testResult =>
                      if Evaluator.isTruthy(testResult) then
                        if items.tail.isEmpty then k(testResult)
                        else Evaluator.evalBodyK(items.tail, env, k)
                      else evalGuardClausesK(rest, env, k, exnVal)
                  )
                )
          case _ => throw new EvalError("guard: malformed clause")
