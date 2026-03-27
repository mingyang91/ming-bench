package ming

class EvalError(message: String) extends Exception(message)

object EvalError:

  def apply(message: String): EvalError =
    new EvalError(message)
