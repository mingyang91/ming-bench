package ming

case class SourcePos(line: Int, col: Int):
  override def toString: String = s"$line:$col"
  def isKnown: Boolean          = line > 0

object SourcePos:
  val None: SourcePos = SourcePos(0, 0)
