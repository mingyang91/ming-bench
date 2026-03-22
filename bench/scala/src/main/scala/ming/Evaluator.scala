package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _) = evalSequence(exprs, Interpreter.defaultEnv)
    lastVal.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  @tailrec
  private def evalSequence(
    exprs: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    exprs match
      case Nil => (SchemeValue.SchemeVoid, env)
      case last :: Nil =>
        Interpreter.eval(last, env)
      case head :: tail =>
        val (_, newEnv) = Interpreter.eval(head, env)
        evalSequence(tail, newEnv)
