package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val (result, _) = evalProgram(input)
    SchemeRenderer.render(result)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val (result, output) = evalProgram(input)
    (SchemeRenderer.render(result), output)

  private def evalProgram(input: String): (Value, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")

    val globalEnv = Env.topLevel()
    val context   = new EvalContext
    val result = MultiValueSupport.requireSingle(
      ExpressionEvaluator.evalSequence(expressions, globalEnv, context),
      expressions.last.pos,
      "top-level expression"
    )
    (result, context.capturedOutput)
