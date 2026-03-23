package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  private def coreBuiltins(output: StringBuilder): List[(String, List[SchemeValue] => SchemeValue)] =
    List(
      ("+", args => Builtins.addOp(args)),
      ("*", args => Builtins.mulOp(args)),
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
      ("number?", args => Builtins.typeCheck(args, Rational.isNumeric)),
      ("boolean?", args => Builtins.typeCheck(args, _.isInstanceOf[BoolVal])),
      ("pair?", args => Builtins.pairCheck(args)),
      ("symbol?", args => Builtins.typeCheck(args, _.isInstanceOf[SymbolVal])),
      ("char?", args => Builtins.typeCheck(args, _.isInstanceOf[CharVal])),
      ("append", args => Builtins.appendOp(args)),
      ("display", args => Builtins.displayOp(args, output)),
      ("write", args => Builtins.writeOp(args, output)),
      ("newline", args => Builtins.newlineOp(args, output)),
      ("string-append", args => BuiltinsExt.stringAppendOp(args)),
      ("string-length", args => BuiltinsExt.stringLengthOp(args)),
      ("substring", args => BuiltinsExt.substringOp(args)),
      ("string->number", args => BuiltinsExt.stringToNumberOp(args)),
      ("number->string", args => BuiltinsExt.numberToStringOp(args)),
      ("symbol->string", args => BuiltinsExt.symbolToStringOp(args)),
      ("string->symbol", args => BuiltinsExt.stringToSymbolOp(args)),
      ("string-ref", args => BuiltinsExt.stringRefOp(args)),
      ("string-set!", args => BuiltinsExt.stringSetOp(args)),
      ("string-copy", args => BuiltinsExt.stringCopyOp(args)),
      ("string->list", args => BuiltinsExt.stringToListOp(args)),
      ("list->string", args => BuiltinsExt.listToStringOp(args)),
      ("char->integer", args => BuiltinsExt.charToIntegerOp(args)),
      ("integer->char", args => BuiltinsExt.integerToCharOp(args)),
      ("apply", args => Builtins.applyOp(args)),
      ("eq?", args => Builtins.eqCheck(args)),
      ("eqv?", args => Builtins.eqvCheck(args)),
      ("equal?", args => Builtins.equalCheck(args)),
      ("dynamic-wind", args => WindException.dynamicWindOp(args)),
      ("raise", args => WindException.raiseOp(args)),
      ("with-exception-handler", args => WindException.withExceptionHandlerOp(args)),
      ("values", args => valuesOp(args)),
      ("call-with-values", args => callWithValuesOp(args))
    )

  private def extBuiltins: List[(String, List[SchemeValue] => SchemeValue)] =
    List(
      ("abs", args => BuiltinsExt.absOp(args)),
      ("modulo", args => BuiltinsExt.moduloOp(args)),
      ("remainder", args => BuiltinsExt.remainderOp(args)),
      ("quotient", args => BuiltinsExt.quotientOp(args)),
      ("min", args => BuiltinsExt.minOp(args)),
      ("max", args => BuiltinsExt.maxOp(args)),
      ("expt", args => BuiltinsExt.exptOp(args)),
      ("zero?", args => BuiltinsExt.zeroCheck(args)),
      ("positive?", args => BuiltinsExt.positiveCheck(args)),
      ("negative?", args => BuiltinsExt.negativeCheck(args)),
      ("odd?", args => BuiltinsExt.oddCheck(args)),
      ("even?", args => BuiltinsExt.evenCheck(args)),
      ("list-ref", args => BuiltinsExt.listRefOp(args)),
      ("list-tail", args => BuiltinsExt.listTailOp(args)),
      ("list?", args => BuiltinsExt.listCheck(args)),
      ("assoc", args => BuiltinsExt.assocOp(args)),
      ("map", args => BuiltinsExt.mapOp(args)),
      ("reverse", args => BuiltinsExt.reverseOp(args)),
      ("char-alphabetic?", args => BuiltinsExt.charAlphabeticCheck(args)),
      ("char-numeric?", args => BuiltinsExt.charNumericCheck(args)),
      ("char-upcase", args => BuiltinsExt.charUpcaseOp(args)),
      ("char-downcase", args => BuiltinsExt.charDowncaseOp(args)),
      ("char=?", args => BuiltinsExt.charEqualCheck(args)),
      ("char<?", args => BuiltinsExt.charLessCheck(args)),
      ("string=?", args => BuiltinsExt.stringEqualCheck(args)),
      ("string<?", args => BuiltinsExt.stringLessCheck(args)),
      ("string-ci=?", args => BuiltinsExt.stringCiEqualCheck(args)),
      ("string-upcase", args => BuiltinsExt.stringUpcaseOp(args)),
      ("string-downcase", args => BuiltinsExt.stringDowncaseOp(args)),
      ("vector", args => BuiltinsVector.vectorOp(args)),
      ("make-vector", args => BuiltinsVector.makeVectorOp(args)),
      ("vector-ref", args => BuiltinsVector.vectorRefOp(args)),
      ("vector-set!", args => BuiltinsVector.vectorSetOp(args)),
      ("vector-length", args => BuiltinsVector.vectorLengthOp(args)),
      ("vector?", args => BuiltinsVector.vectorCheck(args)),
      ("vector->list", args => BuiltinsVector.vectorToListOp(args)),
      ("list->vector", args => BuiltinsVector.listToVectorOp(args)),
      ("exact?", args => Builtins.typeCheck(args, Rational.isExact)),
      ("inexact?", args => Builtins.typeCheck(args, Rational.isInexact)),
      (
        "integer?",
        args =>
          Builtins.typeCheck(
            args,
            {
              case _: IntVal => true
              case _         => false
            }
          )
      ),
      (
        "rational?",
        args =>
          Builtins.typeCheck(
            args,
            {
              case _: IntVal | _: RationalVal => true
              case _                          => false
            }
          )
      ),
      ("exact->inexact", args => exactToInexactOp(args)),
      ("inexact->exact", args => inexactToExactOp(args)),
      ("numerator", args => numeratorOp(args)),
      ("denominator", args => denominatorOp(args))
    )

  private def exactToInexactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil if Rational.isNumeric(v) => DoubleVal(Rational.toDouble(v))
      case _                                 => throw new EvalError("exact->inexact: expected 1 number")

  private def inexactToExactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case DoubleVal(d, _) :: Nil          => Rational.doubleToExact(d)
      case v :: Nil if Rational.isExact(v) => v
      case _                               => throw new EvalError("inexact->exact: expected 1 number")

  private def numeratorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil         => IntVal(n)
      case RationalVal(n, _, _) :: Nil => IntVal(n)
      case _                           => throw new EvalError("numerator: expected exact number")

  private def denominatorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(_, _) :: Nil         => IntVal(1)
      case RationalVal(_, d, _) :: Nil => IntVal(d)
      case _                           => throw new EvalError("denominator: expected exact number")

  private def makeGlobalEnv(output: StringBuilder): Environment =
    val env = Environment()
    (coreBuiltins(output) ++ extBuiltins).foreach { (name, func) =>
      env.define(name, BuiltinVal(name, func))
    }
    val callccFn = callccBuiltin()
    env.define("call/cc", callccFn)
    env.define("call-with-current-continuation", callccFn)
    env

  /** values: single value is transparent, otherwise wrap in ValuesVal. */
  private def valuesOp(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => single
      case _             => ValuesVal(args)

  /** call-with-values: call producer, unpack values, pass to consumer. */
  private def callWithValuesOp(args: List[SchemeValue]): SchemeValue =
    args match
      case producer :: consumer :: Nil =>
        val produced = Interpreter.applyProc(producer, Nil)
        val consumerArgs = produced match
          case ValuesVal(vs) => vs
          case single        => List(single)
        Interpreter.applyProc(consumer, consumerArgs)
      case _ => throw new EvalError("call-with-values: requires 2 arguments")

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
          val contId     = ContinuationManager.freshId()
          val ctx        = ContinuationManager.bodyContext
          val ctxStack   = ContinuationManager.contextStack
          val seqRem     = ContinuationManager.seqRemaining
          val seqE       = ContinuationManager.seqEnv
          val hasSame    = ContinuationManager.hasSameBodyFrame
          val cont       = ContinuationVal(contId, ctx.exprs, ctx.env, ctxStack, seqRem, seqE, hasSame)
          val proc       = args.head
          val savedStack = ContinuationManager.contextStack
          val savedWind  = ContinuationManager.windStack
          try Interpreter.applyProc(proc, List(cont))
          catch
            case jump: ContinuationJump if jump.contId == contId =>
              WindException.doWindTransition(savedWind)
              ContinuationManager.contextStack = savedStack
              jump.value
    )

  /** Evaluate expressions sequentially, setting body context for each. */
  private def evalExprsSequentially(exprs: List[SchemeValue], env: Environment): SchemeValue =
    var result: SchemeValue = Void
    var remaining           = exprs
    while remaining.nonEmpty do
      ContinuationManager.seqRemaining = remaining
      ContinuationManager.seqEnv = env
      ContinuationManager.bodyContext = BodyContext(List(remaining.head), env)
      result = Interpreter.eval(remaining.head, env)
      remaining = remaining.tail
    result

  /** Check if an expression is a direct call/cc application. */
  private def isDirectCallCc(expr: SchemeValue): Boolean = expr match
    case SchemeValue.ListVal(SchemeValue.SymbolVal("call/cc", _) :: _, _)                        => true
    case SchemeValue.ListVal(SchemeValue.SymbolVal("call-with-current-continuation", _) :: _, _) => true
    case _                                                                                       => false

  /** Handle a ContinuationJump by re-evaluating the continuation's saved context. */
  private def handleContinuationJump(jump: ContinuationJump): SchemeValue =
    try
      ContinuationManager.pendingReturn = Some(jump.value)
      if jump.seqRemaining.length > 1 then
        // Multiple top-level expressions: replay from the top-level sequence.
        // This makes values flow through enclosing expressions (e.g., define).
        evalExprsSequentially(jump.seqRemaining, jump.seqEnv)
      else
        // Single top-level expression: replay inner body + context stack frames.
        // Skip the same-body frame when bodyExprs is a direct call/cc expression,
        // to avoid re-executing subsequent call/cc expressions in the same body.
        val outerFrames =
          if jump.hasSameBodyFrame && jump.contextStack.nonEmpty && isDirectCallCc(jump.bodyExprs.head) then
            jump.contextStack.tail
          else jump.contextStack
        val allFrames           = BodyContext(jump.bodyExprs, jump.bodyEnv) :: outerFrames
        var result: SchemeValue = Void
        var remaining           = allFrames
        while remaining.nonEmpty do
          ContinuationManager.contextStack = remaining.tail
          result = evalExprsSequentially(remaining.head.exprs, remaining.head.env)
          remaining = remaining.tail
        result
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
