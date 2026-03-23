package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env     = defaultEnv()
    val results = exprs.map(e => eval(e, env))
    results.filter(_ != SVoid).lastOption.getOrElse(SVoid).display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    Builtins.captureOutput {
      val exprs = Parser.parse(input)
      if exprs.isEmpty then throw new EvalError("no expressions")
      val env     = defaultEnv()
      val results = exprs.map(e => eval(e, env))
      results.filter(_ != SVoid).lastOption.getOrElse(SVoid).display
    }

  private def defaultEnv(): Environment =
    val env = Environment.empty
    def reg(name: String, fn: List[SchemeValue] => SchemeValue): Unit =
      env.define(name, SBuiltin(name, fn))

    reg("+", Builtins.evalAdd)
    reg("-", Builtins.evalSub)
    reg("*", Builtins.evalMul)
    reg("/", Builtins.evalDiv)
    reg("<", args => Builtins.evalCompare(args, _ < _))
    reg(">", args => Builtins.evalCompare(args, _ > _))
    reg("=", args => Builtins.evalCompare(args, _ == _))
    reg("<=", args => Builtins.evalCompare(args, _ <= _))
    reg(">=", args => Builtins.evalCompare(args, _ >= _))
    reg("not", Builtins.evalNot)
    reg("cons", Builtins.evalCons)
    reg("car", Builtins.evalCar)
    reg("cdr", Builtins.evalCdr)
    reg("null?", Builtins.evalNullPred)
    reg("list", Builtins.evalListBuiltin)
    reg("length", Builtins.evalLength)
    reg("append", Builtins.evalAppend)
    reg("string?", args => Builtins.typeCheck(args, _.isInstanceOf[SString]))
    reg("number?", args => Builtins.typeCheck(args, _.isInstanceOf[SInteger]))
    reg(
      "boolean?",
      args => Builtins.typeCheck(args, _.isInstanceOf[SBoolean])
    )
    reg("pair?", args => Builtins.typeCheck(args, _.isInstanceOf[SPair]))
    reg("symbol?", args => Builtins.typeCheck(args, _.isInstanceOf[SSymbol]))
    reg("char?", args => Builtins.typeCheck(args, _.isInstanceOf[SChar]))
    reg("display", Builtins.evalDisplay)
    reg("write", Builtins.evalWrite)
    reg("newline", Builtins.evalNewline)
    reg("string-append", Builtins.evalStringAppend)
    reg("string-length", Builtins.evalStringLength)
    reg("substring", Builtins.evalSubstring)
    reg("string->number", Builtins.evalStringToNumber)
    reg("number->string", Builtins.evalNumberToString)
    reg("symbol->string", Builtins.evalSymbolToString)
    reg("string->symbol", Builtins.evalStringToSymbol)
    reg("string-ref", Builtins.evalStringRef)
    reg("string-set!", Builtins.evalStringSet)
    reg("string-copy", Builtins.evalStringCopy)
    env

  private def withPos(pos: (Int, Int))(body: => SchemeValue): SchemeValue =
    try body
    catch
      case e: EvalError if !e.getMessage.matches(".*\\d+:\\d+.*") =>
        throw new EvalError(s"${e.getMessage} at ${pos._1}:${pos._2}")

  def eval(expr: SchemeValue, env: Environment): SchemeValue = expr match
    case SInteger(_) | SBoolean(_) | SString(_) | SChar(_) | SLambda(_, _, _) | SBuiltin(_, _) | SVoid | SNil |
        SPair(_, _) =>
      expr
    case SSymbol(name) => env.lookup(name)
    case SList(Nil, pos) =>
      throw new EvalError(s"empty application at ${pos._1}:${pos._2}")
    case SList(SSymbol(op) :: args, pos) =>
      withPos(pos) {
        op match
          case "quote"  => evalQuote(args)
          case "if"     => evalIf(args, env)
          case "define" => evalDefine(args, env)
          case "lambda" => evalLambda(args, env)
          case "and"    => evalAnd(args, env)
          case "or"     => evalOr(args, env)
          case "let"    => evalLet(args, env)
          case "begin"  => evalBegin(args, env)
          case "cond"   => evalCond(args, env)
          case _ =>
            val proc       = eval(SSymbol(op), env)
            val evaledArgs = args.map(a => eval(a, env))
            applyProc(proc, evaledArgs)
      }
    case SList(head :: args, pos) =>
      withPos(pos) {
        val proc       = eval(head, env)
        val evaledArgs = args.map(a => eval(a, env))
        applyProc(proc, evaledArgs)
      }

  private def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue = proc match
    case SLambda(params, body, closure) =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      val localEnv = closure.extend(params, args)
      body.map(e => eval(e, localEnv)).last
    case SBuiltin(_, fn) => fn(args)
    case other           => throw new EvalError(s"not a procedure: ${other.display}")

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => quotify(single)
      case _             => throw new EvalError("quote: requires exactly 1 argument")

  private def quotify(v: SchemeValue): SchemeValue = v match
    case SList(elems, _) =>
      elems.foldRight(SNil: SchemeValue)((e, acc) => SPair(quotify(e), acc))
    case other => other

  private def evalIf(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if Builtins.isTruthy(eval(cond, env)) then eval(thenBranch, env)
        else eval(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if Builtins.isTruthy(eval(cond, env)) then eval(thenBranch, env)
        else SVoid
      case _ => throw new EvalError("if: bad syntax")

  private def evalDefine(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case SSymbol(name) :: value :: Nil =>
        env.define(name, eval(value, env))
        SVoid
      case SList(SSymbol(name) :: params, _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SSymbol(n) => n
          case other =>
            throw new EvalError(
              s"define: expected parameter name, got ${other.display}"
            )
        }
        env.define(name, SLambda(paramNames, body, env))
        SVoid
      case _ => throw new EvalError("define: bad syntax")

  private def evalLambda(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case SList(params, _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SSymbol(n) => n
          case other =>
            throw new EvalError(
              s"lambda: expected parameter name, got ${other.display}"
            )
        }
        SLambda(paramNames, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalAnd(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case Nil         => SBoolean(true)
      case last :: Nil => eval(last, env)
      case head :: rest =>
        val v = eval(head, env)
        if !Builtins.isTruthy(v) then v else evalAnd(rest, env)

  private def evalOr(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case Nil         => SBoolean(false)
      case last :: Nil => eval(last, env)
      case head :: rest =>
        val v = eval(head, env)
        if Builtins.isTruthy(v) then v else evalOr(rest, env)

  private def evalLet(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case SSymbol(name) :: SList(bindings, _) :: body if body.nonEmpty =>
        val (paramNames, initVals) = bindings.map {
          case SList(SSymbol(p) :: expr :: Nil, _) => (p, eval(expr, env))
          case other =>
            throw new EvalError(s"let: bad binding: ${other.display}")
        }.unzip
        val localEnv = env.extend(Nil, Nil)
        val lambda   = SLambda(paramNames, body, localEnv)
        localEnv.define(name, lambda)
        applyProc(lambda, initVals)
      case SList(bindings, _) :: body if body.nonEmpty =>
        val (names, vals) = bindings.map {
          case SList(SSymbol(name) :: expr :: Nil, _) =>
            (name, eval(expr, env))
          case other =>
            throw new EvalError(s"let: bad binding: ${other.display}")
        }.unzip
        val localEnv = env.extend(names, vals)
        body.map(e => eval(e, localEnv)).last
      case _ => throw new EvalError("let: bad syntax")

  private def evalBegin(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    if args.isEmpty then SVoid
    else args.map(e => eval(e, env)).last

  private def evalCond(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case Nil => SVoid
      case SList(SSymbol("else") :: body, _) :: Nil =>
        if body.isEmpty then throw new EvalError("cond: else clause must have body")
        body.map(e => eval(e, env)).last
      case SList(test :: body, _) :: rest =>
        if Builtins.isTruthy(eval(test, env)) then
          if body.isEmpty then eval(test, env)
          else body.map(e => eval(e, env)).last
        else evalCond(rest, env)
      case _ => throw new EvalError("cond: bad syntax")
