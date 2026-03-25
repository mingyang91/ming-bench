package ming

private[ming] object SchemeBuiltins:
  import SchemeInterpreter.Value

  def all(writeOutput: String => Unit): List[Value.Builtin] =
    List.concat(
      NumericBuiltins.all,
      LogicalBuiltins.all,
      EqualityBuiltins.all,
      ListBuiltins.all,
      VectorBuiltins.all,
      CharBuiltins.all,
      StringBuiltins.all,
      SyntaxBuiltins.all,
      IoBuiltins.all(writeOutput),
      PredicateBuiltins.all
    )
