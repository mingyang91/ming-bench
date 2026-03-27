package ming

private[ming] object Level1Interpreter:

  def evalProgram(input: String): String =
    evalProgramValue(input).render

  def evalProgramWithLimit(input: String, maxSteps: Int): String =
    evalProgramValueWithLimit(input, maxSteps).render

  def evalProgramWithOutput(input: String): (String, String) =
    val (result, output) = OutputCapture.capture(evalProgramValue(input))
    (result.render, output)

  private def evalProgramValue(input: String): Value =
    evalProgramValue(input, stepLimit = None)

  private def evalProgramValueWithLimit(input: String, maxSteps: Int): Value =
    evalProgramValue(input, stepLimit = Some(maxSteps))

  private def evalProgramValue(input: String, stepLimit: Option[Int]): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then SchemeFailure.raise("expected expression", Position(1, 1))

    val env = Environment.root(Level1Builtins.values)
    stepLimit match
      case Some(maxSteps) =>
        StepLimitRuntime.withStepLimit(maxSteps)(InterpreterEvaluator.evalSequence(expressions, env))
      case None =>
        InterpreterEvaluator.evalSequence(expressions, env)
