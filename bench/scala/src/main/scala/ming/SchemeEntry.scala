package ming

/** Public entry points for evaluating Scheme strings. */
object SchemeEntry:

  def evalStr(input: String): String =
    val parser = SchemeParser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = BuiltinRegistry.makeTopLevelEnv()
    Display.display(exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => Evaluator.eval(e, env)))

  def evalStrWithOutput(input: String): (String, String) =
    val parser = SchemeParser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = BuiltinRegistry.makeTopLevelEnv()
    val buf = new StringBuilder
    Builtins.outputBuffer.set(buf)
    try
      val result = Display.display(exprs.foldLeft(Expr.Bool(false): Expr)((_, e) => Evaluator.eval(e, env)))
      (result, buf.toString)
    finally Builtins.outputBuffer.remove()
