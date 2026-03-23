package ming

/** Source position (1-based line and column). */
case class SourcePos(line: Int, col: Int):
  override def toString: String = s"$line:$col"
