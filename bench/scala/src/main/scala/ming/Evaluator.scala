package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    Level1Interpreter.evalProgram(input)

  /** Evaluate one or more Scheme expressions with a maximum number of expression-dispatch steps.
    */
  def evalStrWithLimit(input: String, maxSteps: Int): String =
    Level1Interpreter.evalProgramWithLimit(input, maxSteps)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    Level1Interpreter.evalProgramWithOutput(input)
