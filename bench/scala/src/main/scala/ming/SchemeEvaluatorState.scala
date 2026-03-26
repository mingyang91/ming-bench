package ming

import SchemeModel.*

private[ming] object SchemeEvaluatorState:

  sealed trait Computation

  object Computation:
    final case class Done(value: Value)               extends Computation
    final case class Suspend(step: () => Computation) extends Computation

  type Continuation = Value => Computation

  def done(value: Value): Computation =
    Computation.Done(value)

  def suspend(step: => Computation): Computation =
    Computation.Suspend(() => step)

  def resume(continuation: Continuation, value: Value): Computation =
    suspend(continuation(value))
