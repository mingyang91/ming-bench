package ming

class EvalError(
  val baseMessage: String,
  val sourcePos: SourcePos = SourcePos.None
) extends Exception(
      if sourcePos.isKnown then s"$baseMessage [${sourcePos}]" else baseMessage
    )
