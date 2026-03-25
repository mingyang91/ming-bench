package ming

private[ming] object IoBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all(writeOutput: String => Unit): List[Value.Builtin] =
    List(
      displayBuiltin(writeOutput),
      writeBuiltin(writeOutput),
      newlineBuiltin(writeOutput)
    )

  private def displayBuiltin(writeOutput: String => Unit): Value.Builtin =
    Value.Builtin(
      "display",
      (args, pos) =>
        writeOutput(SchemeInterpreter.renderDisplay(singleArg("display", args, pos)))
        Value.Void
    )

  private def writeBuiltin(writeOutput: String => Unit): Value.Builtin =
    Value.Builtin(
      "write",
      (args, pos) =>
        writeOutput(SchemeInterpreter.render(singleArg("write", args, pos)))
        Value.Void
    )

  private def newlineBuiltin(writeOutput: String => Unit): Value.Builtin =
    Value.Builtin(
      "newline",
      (args, pos) =>
        requireExactly("newline", args, expected = 0, pos)
        writeOutput("\n")
        Value.Void
    )
