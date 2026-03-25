package ming

final private[ming] class Runtime private ():
  import SchemeInterpreter.{DynamicWindFrame, ExceptionHandlerFrame}

  private val output              = new StringBuilder
  private var currentWindStack    = Vector.empty[DynamicWindFrame]
  private var currentHandlerStack = Vector.empty[ExceptionHandlerFrame]

  def emit(text: String): Unit =
    output.append(text)

  def capturedOutput: String =
    output.toString

  def windStack: Vector[DynamicWindFrame] =
    currentWindStack

  def handlerStack: Vector[ExceptionHandlerFrame] =
    currentHandlerStack

  def pushWindFrame(frame: DynamicWindFrame): Unit =
    currentWindStack = currentWindStack :+ frame

  def popWindFrame(expected: DynamicWindFrame): Unit =
    if currentWindStack.isEmpty || !(currentWindStack.last eq expected) then
      throw new IllegalStateException("dynamic-wind stack mismatch")
    currentWindStack = currentWindStack.dropRight(1)

  def setWindStack(stack: Vector[DynamicWindFrame]): Unit =
    currentWindStack = stack

  def pushExceptionHandler(frame: ExceptionHandlerFrame): Unit =
    currentHandlerStack = currentHandlerStack :+ frame

  def popExceptionHandler(expected: ExceptionHandlerFrame): Unit =
    if currentHandlerStack.isEmpty || !(currentHandlerStack.last eq expected) then
      throw new IllegalStateException("exception handler stack mismatch")
    currentHandlerStack = currentHandlerStack.dropRight(1)

  def setHandlerStack(stack: Vector[ExceptionHandlerFrame]): Unit =
    currentHandlerStack = stack

private[ming] object Runtime:
  def apply(): Runtime = new Runtime()
