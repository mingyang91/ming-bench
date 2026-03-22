package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = builtinEnv
    val result = exprs.foldLeft((SchemeValue.Void: SchemeValue, env)) { case ((_, e), expr) =>
      (Interpreter.eval(expr, e), e)
    }
    result._1.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")

  private val builtinNames: List[String] = List(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not"
  )

  private val builtinEnv: Map[String, SchemeValue] =
    builtinNames.map(n => n -> SchemeValue.SymbolVal(n)).toMap
