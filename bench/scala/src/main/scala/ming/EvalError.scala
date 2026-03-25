package ming

class EvalError(message: String) extends Exception(message)

object EvalError:

  def at(pos: SourcePos, message: String): EvalError =
    new EvalError(s"$pos: $message")
