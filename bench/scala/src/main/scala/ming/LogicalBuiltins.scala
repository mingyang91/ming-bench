package ming

private[ming] object LogicalBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(notBuiltin)

  private val notBuiltin: Value.Builtin =
    Value.Builtin(
      "not",
      (args, pos) => Value.Bool(!isTruthy(singleArg("not", args, pos)))
    )
