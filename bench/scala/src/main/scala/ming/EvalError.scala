package ming

class EvalError(message: String) extends Exception(message)

object EvalError:

  def withPos(msg: String, pos: Option[(Int, Int)]): EvalError =
    val prefix = pos.map { case (l, c) => s"$l:$c: " }.getOrElse("")
    new EvalError(s"$prefix$msg")
