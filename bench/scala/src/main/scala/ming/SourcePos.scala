package ming

final case class SourcePos(line: Int, column: Int):

  override def toString: String =
    s"$line:$column"
