package ming

import ContinuationFrame.*

private[ming] object MachineExceptions:

  def raise(machine: Machine, exception: Value, pos: SourcePos): Unit =
    machine.handlers.headOption match
      case Some(handler) =>
        machine.handlers = handler.outerHandlers

        val sharedPrefix = sharedWindPrefix(machine.winds, handler.outerWinds)
        val exiting      = machine.winds.drop(sharedPrefix).reverse
        val entering     = handler.outerWinds.drop(sharedPrefix)
        continueRaiseAfterExit(machine, exiting, entering, handler, exception, pos)
      case None =>
        throw EvalError.at(pos, s"unhandled exception: ${SchemeRenderer.render(exception)}")

  def continueRaiseAfterExit(
    machine: Machine,
    remaining: List[DynamicWindContext],
    entering: List[DynamicWindContext],
    handler: ExceptionHandlerContext,
    exception: Value,
    pos: SourcePos
  ): Unit =
    remaining match
      case wind :: tail =>
        machine.deactivateWind(wind)
        machine.push(ExceptionWindExit(tail, entering, handler, exception, pos))
        machine.setInvoke(wind.outThunk, Nil, wind.pos)
      case Nil =>
        continueRaiseAfterEnter(machine, entering, handler, exception, pos)

  def continueRaiseAfterEnter(
    machine: Machine,
    remaining: List[DynamicWindContext],
    handler: ExceptionHandlerContext,
    exception: Value,
    pos: SourcePos
  ): Unit =
    remaining match
      case wind :: tail =>
        machine.push(ExceptionWindEnter(wind, tail, handler, exception, pos))
        machine.setInvoke(wind.inThunk, Nil, wind.pos)
      case Nil =>
        machine.frames = ExceptionHandlerReturned(pos) :: handler.outerFrames
        machine.winds = handler.outerWinds
        machine.handlers = handler.outerHandlers
        machine.setInvoke(handler.handler, List(exception), handler.pos)

  @annotation.tailrec
  private def sharedWindPrefix(
    current: List[DynamicWindContext],
    target: List[DynamicWindContext],
    shared: Int = 0
  ): Int =
    (current, target) match
      case (currentHead :: currentTail, targetHead :: targetTail) if currentHead.eq(targetHead) =>
        sharedWindPrefix(currentTail, targetTail, shared + 1)
      case _ =>
        shared
