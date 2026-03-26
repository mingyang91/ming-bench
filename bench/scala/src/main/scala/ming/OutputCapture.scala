package ming

import scala.util.DynamicVariable

private[ming] object OutputCapture:
  private val current = DynamicVariable(Option.empty[StringBuilder])

  def capture[A](thunk: => A): (A, String) =
    val buffer = StringBuilder()
    val result = current.withValue(Some(buffer))(thunk)
    (result, buffer.result())

  def append(text: String): Unit =
    current.value.foreach(_.append(text))
