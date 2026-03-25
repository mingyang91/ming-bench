package ming

final private[ming] class RuntimeContext:

  private val output = new StringBuilder

  def appendOutput(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString
