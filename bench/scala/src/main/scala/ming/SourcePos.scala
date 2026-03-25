package ming

final private[ming] case class SourcePos(line: Int, col: Int):

  override def toString: String = s"$line:$col"
