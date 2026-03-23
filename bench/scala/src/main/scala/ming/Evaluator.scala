package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  private def makeGlobalEnv(): Environment =
    val env = Environment()
    val builtins: List[(String, List[SchemeValue] => SchemeValue)] = List(
      ("+", args => Interpreter.arith(args, _ + _, 0)),
      ("*", args => Interpreter.arith(args, _ * _, 1)),
      ("-", args => Interpreter.subtractOp(args)),
      ("/", args => Interpreter.divideOp(args)),
      ("<", args => Interpreter.compare(args, _ < _)),
      (">", args => Interpreter.compare(args, _ > _)),
      ("=", args => Interpreter.compare(args, _ == _)),
      ("<=", args => Interpreter.compare(args, _ <= _)),
      (">=", args => Interpreter.compare(args, _ >= _)),
      ("not", args => Interpreter.notOp(args))
    )
    builtins.foreach { (name, func) =>
      env.define(name, BuiltinVal(name, func))
    }
    env

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env     = makeGlobalEnv()
    val results = exprs.map(Interpreter.eval(_, env))
    val last    = results.last
    last match
      case Void => ""
      case _    => last.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
