package ming

private[ming] object Builtins:

  def globalEnv(runtime: RuntimeContext, macros: MacroState): Map[String, Value] =
    NumericBuiltins.entries ++
      PredicateBuiltins.entries ++
      ListBuiltins.entries(macros) ++
      VectorBuiltins.entries ++
      TextBuiltins.entries(runtime)
