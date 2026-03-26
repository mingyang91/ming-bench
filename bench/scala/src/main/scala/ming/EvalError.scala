package ming

class EvalError(message: String) extends Exception(message)

class SchemeRaisedException(val value: SchemeVal) extends RuntimeException("scheme raise")
