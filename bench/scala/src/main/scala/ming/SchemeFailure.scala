package ming

private[ming] object SchemeFailure:

  def raise(message: String, position: Position): Nothing =
    throw new EvalError(s"$message at ${position.line}:${position.column}")
