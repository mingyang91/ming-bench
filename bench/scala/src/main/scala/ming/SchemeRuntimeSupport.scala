package ming

private[ming] object SchemeRuntimeSupport:
  import SchemeInterpreter.Value

  def initialEnv(runtime: Runtime): Env =
    val env = Env.root()
    SchemeBuiltins.all(runtime.emit).foreach { builtin =>
      env.define(builtin.name, builtin)
    }
    env.define("apply", Value.ApplyProcedureBuiltin)
    env.define("map", Value.MapProcedureBuiltin)
    env.define("for-each", Value.ForEachProcedureBuiltin)
    env.define("call/cc", Value.CallWithCurrentContinuation)
    env.define("call-with-current-continuation", Value.CallWithCurrentContinuation)
    env.define("dynamic-wind", Value.DynamicWindBuiltin)
    env
