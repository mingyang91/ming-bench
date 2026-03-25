package ming

private[ming] object RuntimeBuiltins:

  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(errorBuiltin)

  private val errorBuiltin: Value.Builtin =
    Value.Builtin(
      "error",
      (args, pos) =>
        val message =
          if args.isEmpty then "error"
          else args.map(SchemeInterpreter.renderDisplay).mkString(" ")
        throw EvalError.at(pos, message)
    )
