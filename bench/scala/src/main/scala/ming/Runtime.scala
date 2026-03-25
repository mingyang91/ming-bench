package ming

final private[ming] class Runtime private ():
  private val output = new StringBuilder

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString

private[ming] object Runtime:
  def apply(): Runtime = new Runtime()
