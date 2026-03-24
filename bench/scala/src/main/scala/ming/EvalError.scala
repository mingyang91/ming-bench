package ming

class EvalError(message: String) extends Exception(message)

class SchemeRaise(val value: SchemeVal) extends Throwable(null, null, true, false):
  override def fillInStackTrace(): Throwable = this
