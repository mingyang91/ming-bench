package ming

import Evaluator.{Bounce, Cont, More}

/** CPS handlers for raise, with-exception-handler, and call-with-values. */
object ExceptionForms:

  def evalRaiseK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size != 1 then throw new EvalError("raise: expected 1 argument")
    Evaluator.evalK(
      args.head,
      env,
      value =>
        if Evaluator.exceptionHandlers.isEmpty then
          throw new EvalError(s"unhandled exception: ${SchemeVal.display(value)}")
        val handler = Evaluator.exceptionHandlers.head
        Evaluator.exceptionHandlers = Evaluator.exceptionHandlers.tail
        handler(value)
    )

  def evalWithExceptionHandlerK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
    Evaluator.evalK(
      args(0),
      env,
      handlerProc =>
        Evaluator.evalK(
          args(1),
          env,
          thunkProc =>
            val savedHandlers = Evaluator.exceptionHandlers
            val cpsHandler: Evaluator.ExceptionHandler =
              value => Evaluator.applyK(handlerProc, List(value), _ => throw new EvalError("raise: handler returned"))
            Evaluator.exceptionHandlers = cpsHandler :: Evaluator.exceptionHandlers
            Evaluator.applyK(
              thunkProc,
              Nil,
              result =>
                Evaluator.exceptionHandlers = savedHandlers
                k(result)
            )
        )
    )

  def evalCallWithValuesK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size != 2 then throw new EvalError("call-with-values: expected 2 arguments")
    Evaluator.evalK(
      args(0),
      env,
      producer =>
        Evaluator.evalK(
          args(1),
          env,
          consumer =>
            Evaluator.applyK(
              producer,
              Nil,
              result =>
                result match
                  case SchemeVal.MultipleValues(vals) => Evaluator.applyK(consumer, vals, k)
                  case other                          => Evaluator.applyK(consumer, List(other), k)
            )
        )
    )
