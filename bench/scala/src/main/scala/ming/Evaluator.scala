package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  private def makeGlobalEnv(output: StringBuilder): Environment =
    val env = Environment()
    val builtins: List[(String, List[SchemeValue] => SchemeValue)] = List(
      ("+", args => Builtins.arith(args, _ + _, 0)),
      ("*", args => Builtins.arith(args, _ * _, 1)),
      ("-", args => Builtins.subtractOp(args)),
      ("/", args => Builtins.divideOp(args)),
      ("<", args => Builtins.compare(args, _ < _)),
      (">", args => Builtins.compare(args, _ > _)),
      ("=", args => Builtins.compare(args, _ == _)),
      ("<=", args => Builtins.compare(args, _ <= _)),
      (">=", args => Builtins.compare(args, _ >= _)),
      ("not", args => Builtins.notOp(args)),
      ("cons", args => Builtins.consOp(args)),
      ("car", args => Builtins.carOp(args)),
      ("cdr", args => Builtins.cdrOp(args)),
      ("null?", args => Builtins.nullCheck(args)),
      ("list", args => ListVal(args)),
      ("length", args => Builtins.lengthOp(args)),
      ("string?", args => Builtins.typeCheck(args, v => v.isInstanceOf[StringVal] || v.isInstanceOf[MutableStringVal])),
      ("number?", args => Builtins.typeCheck(args, _.isInstanceOf[IntVal])),
      ("boolean?", args => Builtins.typeCheck(args, _.isInstanceOf[BoolVal])),
      ("pair?", args => Builtins.pairCheck(args)),
      ("symbol?", args => Builtins.typeCheck(args, _.isInstanceOf[SymbolVal])),
      ("char?", args => Builtins.typeCheck(args, _.isInstanceOf[CharVal])),
      ("append", args => Builtins.appendOp(args)),
      ("display", args => Builtins.displayOp(args, output)),
      ("write", args => Builtins.writeOp(args, output)),
      ("newline", args => Builtins.newlineOp(args, output)),
      ("string-append", args => Builtins.stringAppendOp(args)),
      ("string-length", args => Builtins.stringLengthOp(args)),
      ("substring", args => Builtins.substringOp(args)),
      ("string->number", args => Builtins.stringToNumberOp(args)),
      ("number->string", args => Builtins.numberToStringOp(args)),
      ("symbol->string", args => Builtins.symbolToStringOp(args)),
      ("string->symbol", args => Builtins.stringToSymbolOp(args)),
      ("string-ref", args => Builtins.stringRefOp(args)),
      ("string-set!", args => Builtins.stringSetOp(args)),
      ("string-copy", args => Builtins.stringCopyOp(args)),
      ("apply", args => Builtins.applyOp(args))
    )
    builtins.foreach { (name, func) =>
      env.define(name, BuiltinVal(name, func))
    }
    val callccFn = callccBuiltin()
    env.define("call/cc", callccFn)
    env.define("call-with-current-continuation", callccFn)
    env

  /** Create the call/cc builtin function. */
  private def callccBuiltin(): SchemeValue =
    BuiltinVal(
      "call/cc",
      args =>
        if args.length != 1 then throw new EvalError("call/cc: requires 1 argument")
        val pending = ContinuationManager.pendingReturn
        if pending.isDefined then
          ContinuationManager.pendingReturn = None
          pending.get
        else
          val contId = ContinuationManager.freshId()
          val ctx    = ContinuationManager.bodyContext
          val cont   = ContinuationVal(contId, ctx.exprs, ctx.env)
          val proc   = args.head
          try Interpreter.applyProc(proc, List(cont))
          catch
            case jump: ContinuationJump if jump.contId == contId =>
              jump.value
    )

  /** Evaluate expressions sequentially, setting body context for each. */
  private def evalExprsSequentially(exprs: List[SchemeValue], env: Environment): SchemeValue =
    var result: SchemeValue = Void
    var remaining           = exprs
    while remaining.nonEmpty do
      ContinuationManager.bodyContext = BodyContext(remaining, env)
      result = Interpreter.eval(remaining.head, env)
      remaining = remaining.tail
    result

  /** Handle a ContinuationJump by re-evaluating the continuation's saved body. */
  private def handleContinuationJump(jump: ContinuationJump): SchemeValue =
    try
      ContinuationManager.pendingReturn = Some(jump.value)
      evalExprsSequentially(jump.bodyExprs, jump.bodyEnv)
    catch case jump2: ContinuationJump => handleContinuationJump(jump2)

  private def formatResult(value: SchemeValue): String =
    value match
      case Void => ""
      case _    => value.display

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val output = StringBuilder()
    val exprs  = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env = makeGlobalEnv(output)
    ContinuationManager.reset()
    val result =
      try evalExprsSequentially(exprs, env)
      catch case jump: ContinuationJump => handleContinuationJump(jump)
    formatResult(result)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val output = StringBuilder()
    val exprs  = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env = makeGlobalEnv(output)
    ContinuationManager.reset()
    val result =
      try evalExprsSequentially(exprs, env)
      catch case jump: ContinuationJump => handleContinuationJump(jump)
    (formatResult(result), output.toString)
