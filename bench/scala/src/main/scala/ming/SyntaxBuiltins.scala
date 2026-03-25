package ming

private[ming] object SyntaxBuiltins:

  import BuiltinSupport.{singleArg, twoArgs}
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      syntaxToDatumBuiltin,
      datumToSyntaxBuiltin
    )

  private val syntaxToDatumBuiltin: Value.Builtin =
    Value.Builtin(
      "syntax->datum",
      (args, pos) =>
        val value = singleArg("syntax->datum", args, pos)
        SchemeSyntax.syntaxToDatum(value, pos, "syntax->datum")
    )

  private val datumToSyntaxBuiltin: Value.Builtin =
    Value.Builtin(
      "datum->syntax",
      (args, pos) =>
        val (contextValue, datumValue) = twoArgs("datum->syntax", args, pos)
        SchemeSyntax.datumToSyntax(contextValue, datumValue, pos, "datum->syntax")
    )
