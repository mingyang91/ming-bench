package ming

import scala.collection.mutable

private[ming] object GlobalEnv:

  def create(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(mutable.Map.empty, None)
    Builtins.install(env, output)
    env.set("call/cc", SchemeCallCC)
    env.set("call-with-current-continuation", SchemeCallCC)
    env.set("dynamic-wind", SchemeDynamicWind)
    env.set("with-exception-handler", SchemeWithExceptionHandler)
    env.set("call-with-values", SchemeCallWithValues)
    env.set(
      "values",
      SchemeBuiltin(
        "values",
        args =>
          if args.size == 1 then args.head
          else SchemeValues(args)
      )
    )
    env.set(
      "raise",
      SchemeBuiltin(
        "raise",
        args =>
          if args.size != 1 then throw new EvalError("raise: expected 1 argument")
          throw new SchemeRaisedException(args.head)
      )
    )
    env
