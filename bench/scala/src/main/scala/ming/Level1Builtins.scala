package ming

private[ming] object Level1Builtins:

  val values: Map[String, Value] =
    Level1NumericBuiltins.values ++
      Level1ListBuiltins.values ++
      ContinuationBuiltins.values ++
      Level1VectorBuiltins.values ++
      Level1StringBuiltins.values ++
      Level1PredicateBuiltins.values ++
      SyntaxBuiltins.values
