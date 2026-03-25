package ming

private[ming] object SchemeRuntimeSupport:
  import SchemeInterpreter.Value

  def initialEnv(runtime: Runtime): Env =
    val env = Env.root()
    SchemeBuiltins.all(runtime.emit).foreach { builtin =>
      env.define(builtin.name, builtin)
    }
    env.define("apply", applyBuiltin)
    env

  private val applyBuiltin: Value.Builtin =
    Value.Builtin(
      "apply",
      (args, pos) =>
        BuiltinSupport.requireAtLeast("apply", args, expected = 2, pos)
        val procedure = args.head
        val prefix    = args.tail.dropRight(1)
        val rest      = BuiltinSupport.asList(args.last, "apply", pos)
        SchemeInterpreter.applyProcedure(procedure, prefix ++ rest, pos)
    )
