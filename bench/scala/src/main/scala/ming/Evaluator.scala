package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _, _) = evalSequence(exprs, Interpreter.defaultEnv, "")
    lastVal.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (lastVal, _, output) =
      evalSequence(exprs, Interpreter.defaultEnv, "")
    (lastVal.display, output)

  @tailrec
  private def evalSequence(
    exprs: List[SchemeValue],
    env: Environment,
    accOut: String
  ): (SchemeValue, Environment, String) =
    exprs match
      case Nil => (SchemeValue.SchemeVoid, env, accOut)
      case last :: Nil =>
        val (v, e, o) = Interpreter.eval(last, env)
        (v, e, accOut + o)
      case head :: tail =>
        val (_, newEnv, o) = Interpreter.eval(head, env)
        evalSequence(tail, newEnv, accOut + o)
