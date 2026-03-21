package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val (state, value) = SchemeInterpreter.evaluateProgram(input)
    value.render(state)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val (state, value) = SchemeInterpreter.evaluateProgram(input)
    (value.render(state), state.renderedOutput)
