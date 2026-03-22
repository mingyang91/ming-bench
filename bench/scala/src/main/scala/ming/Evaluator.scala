package ming

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val out         = Array("")
    val (result, _) = evalProgram(exprs, builtinEnv, out)
    result.display

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val out         = Array("")
    val (result, _) = evalProgram(exprs, builtinEnv, out)
    (result.display, out(0))

  private def evalProgram(
    exprs: List[SchemeValue],
    env: Map[String, SchemeValue],
    out: Array[String]
  ): (SchemeValue, Map[String, SchemeValue]) =
    val k: CpsEval.Cont = (v, e) => Bounce.Done(v, e)
    val bounce          = CpsEval.evalSequenceK(exprs, env, out, k)
    CpsEval.run(bounce)

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
    "char?",
    "apply",
    "call/cc",
    "call-with-current-continuation",
    // L13: equality
    "eq?",
    "eqv?",
    "equal?",
    // L13: numeric
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "min",
    "max",
    "expt",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    // L13: list
    "list-ref",
    "list-tail",
    "list?",
    "assoc",
    "map",
    "for-each",
    // L13: char
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?",
    // L13: string
    "string=?",
    "string<?",
    "string-ci=?",
    "string-upcase",
    "string-downcase",
    // L14: string/list and char/integer conversion
    "string->list",
    "list->string",
    "char->integer",
    "integer->char"
  )

  private val builtinEnv: Map[String, SchemeValue] =
    builtinNames.map(n => n -> SchemeValue.Cell(Array(SchemeValue.SymbolVal(n)))).toMap
