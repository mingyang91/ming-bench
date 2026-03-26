package ming

class EvalError private[ming] (val detail: String, val position: Option[SourcePos] = None)
    extends Exception(
      position match
        case Some(pos) => s"$detail at $pos"
        case None      => detail
    ):

  def withPosition(pos: SourcePos): EvalError =
    position match
      case Some(_) => this
      case None    => EvalError.at(detail, pos)

object EvalError:

  def at(message: String, position: SourcePos): EvalError =
    new EvalError(message, Some(position))
