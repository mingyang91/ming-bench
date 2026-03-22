package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = builtinEnv
    val (result, _, _) = exprs.foldLeft((SchemeValue.Void: SchemeValue, env, "")) { case ((_, e, accOut), expr) =>
      val (v, newE, o) = Interpreter.eval(expr, e)
      (v, newE, accOut + o)
    }
    result.display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = builtinEnv
    val (result, _, output) = exprs.foldLeft((SchemeValue.Void: SchemeValue, env, "")) { case ((_, e, accOut), expr) =>
      val (v, newE, o) = Interpreter.eval(expr, e)
      (v, newE, accOut + o)
    }
    (result.display, output)

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
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "pair?",
    "number?",
    "boolean?",
    "string?",
    "symbol?",
    "append",
    "display",
    "write",
    "newline",
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "char?"
  )

  private val builtinEnv: Map[String, SchemeValue] =
    builtinNames.map(n => n -> SchemeValue.Cell(Array(SchemeValue.SymbolVal(n)))).toMap
