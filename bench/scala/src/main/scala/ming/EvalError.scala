package ming

class EvalError private (
  val detail: String,
  val position: Option[SourcePos]
) extends Exception(
      position match
        case Some(pos) => s"$pos: $detail"
        case None      => detail
    )

object EvalError:

  def apply(message: String): EvalError =
    new EvalError(message, None)

  def at(position: SourcePos, message: String): EvalError =
    new EvalError(message, Some(position))
