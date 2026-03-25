package ming

private[ming] object SchemeBuiltins:
  import SchemeInterpreter.Value

  def all(writeOutput: String => Unit): List[Value.Builtin] =
    List.concat(
      NumericBuiltins.all,
      LogicalBuiltins.all,
      ListBuiltins.all,
      StringBuiltins.all,
      IoBuiltins.all(writeOutput),
      PredicateBuiltins.all
    )
