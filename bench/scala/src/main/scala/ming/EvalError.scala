package ming

final class EvalError(message: String) extends Exception(message)

object EvalError:

  def apply(message: String): EvalError =
    new EvalError(message)

  def at(position: SourcePos, message: String): EvalError =
    EvalError(s"${position.line}:${position.column}: $message")
