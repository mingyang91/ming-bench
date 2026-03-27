package ming

private[ming] object SyntaxBuiltins:

  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "syntax->datum" -> BuiltinValue("syntax->datum", syntaxToDatum),
    "datum->syntax" -> BuiltinValue("datum->syntax", datumToSyntax)
  )

  private def syntaxToDatum(arguments: List[Value], position: Position): Value =
    val syntaxObject =
      expectSyntaxObject(expectSingleArgument(arguments, "syntax->datum", position), "syntax->datum", position)
    SpecialFormQuoteEvaluator.quote(syntaxObject.expr)

  private def datumToSyntax(arguments: List[Value], position: Position): Value =
    val (contextValue, datumValue) = expectTwoArguments(arguments, "datum->syntax", position)
    expectSyntaxObject(contextValue, "datum->syntax", position)
    SyntaxObjectValue(MacroExpansion(SyntaxSupport.datumToExpr(datumValue, position), Nil))
