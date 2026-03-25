package ming

private[ming] object Builtins:

  def globalEnv(runtime: RuntimeContext): Map[String, Value] =
    NumericBuiltins.entries ++
      PredicateBuiltins.entries ++
      ListBuiltins.entries ++
      TextBuiltins.entries(runtime)
