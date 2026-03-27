package ming

final private[ming] class EvalContext:

  private val output = new StringBuilder

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString
