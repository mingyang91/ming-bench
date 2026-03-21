package ming

final class EvalError(message: String) extends Exception(message)

object EvalError:

  def syntax(message: String): EvalError =
    new EvalError(s"syntax error: $message")

  def unboundSymbol(name: String): EvalError =
    new EvalError(s"unbound symbol: $name")

  def wrongArgCount(name: String, expected: String, actual: Int): EvalError =
    new EvalError(s"$name expected $expected arguments, got $actual")

  def typeMismatch(expected: String, actual: SchemeValue): EvalError =
    new EvalError(s"type mismatch: expected $expected, got ${actual.render}")

  def divisionByZero(): EvalError =
    new EvalError("division by zero")

  def notAProcedure(): EvalError =
    new EvalError("attempted to call a non-procedure")
