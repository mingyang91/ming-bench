package ming

class EvalError(message: String, val hasPosition: Boolean = false) extends Exception(message)
