package ming

private[ming] object Level1Interpreter:

  def evalProgram(input: String): String =
    evalProgramValue(input).render

  def evalProgramWithOutput(input: String): (String, String) =
    val (result, output) = OutputCapture.capture(evalProgramValue(input))
    (result.render, output)

  private def evalProgramValue(input: String): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then SchemeFailure.raise("expected expression", Position(1, 1))

    val env = Environment.root(Level1Builtins.values)
    InterpreterEvaluator.evalSequence(expressions, env)
