package ming

final private[ming] class Runtime private ():
  import SchemeInterpreter.DynamicWindFrame

  private val output           = new StringBuilder
  private var currentWindStack = Vector.empty[DynamicWindFrame]

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString

  def windStack: Vector[DynamicWindFrame] =
    currentWindStack

  def pushWindFrame(frame: DynamicWindFrame): Unit =
    currentWindStack = currentWindStack :+ frame

  def popWindFrame(expected: DynamicWindFrame): Unit =
    if currentWindStack.isEmpty || !(currentWindStack.last eq expected) then
      throw new IllegalStateException("dynamic-wind stack mismatch")
    currentWindStack = currentWindStack.dropRight(1)

  def setWindStack(stack: Vector[DynamicWindFrame]): Unit =
    currentWindStack = stack

private[ming] object Runtime:
  def apply(): Runtime = new Runtime()
