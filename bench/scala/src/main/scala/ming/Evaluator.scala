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
    val result = evalStr(input)
    (result, "")

  private def defaultEnv(): Environment =
    val env = Environment.empty
    def reg(name: String, fn: List[SchemeValue] => SchemeValue): Unit =
      env.define(name, SBuiltin(name, fn))

    reg("+", evalAdd)
    reg("-", evalSub)
    reg("*", evalMul)
    reg("/", evalDiv)
    reg("<", args => evalCompare(args, _ < _))
    reg(">", args => evalCompare(args, _ > _))
    reg("=", args => evalCompare(args, _ == _))
    reg("<=", args => evalCompare(args, _ <= _))
    reg(">=", args => evalCompare(args, _ >= _))
    reg("not", evalNot)
    reg("cons", evalCons)
    reg("car", evalCar)
    reg("cdr", evalCdr)
    reg("null?", evalNullPred)
    reg("list", evalListBuiltin)
    reg("length", evalLength)
    reg("append", evalAppend)
    reg("string?", args => typeCheck(args, _.isInstanceOf[SString]))
    reg("number?", args => typeCheck(args, _.isInstanceOf[SInteger]))
    reg("boolean?", args => typeCheck(args, _.isInstanceOf[SBoolean]))
    reg("pair?", args => typeCheck(args, _.isInstanceOf[SPair]))
    reg("symbol?", args => typeCheck(args, _.isInstanceOf[SSymbol]))
    env

  def eval(expr: SchemeValue, env: Environment): SchemeValue = expr match
    case SInteger(_) | SBoolean(_) | SString(_) | SLambda(_, _, _) | SBuiltin(_, _) | SVoid | SNil | SPair(_, _) => expr
    case SSymbol(name) => env.lookup(name)
    case SList(Nil)    => throw new EvalError("empty application")
    case SList(SSymbol(op) :: args) =>
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
    case SList(head :: args) =>
      val proc       = eval(head, env)
      val evaledArgs = args.map(a => eval(a, env))
      applyProc(proc, evaledArgs)

  private def applyProc(proc: SchemeValue, args: List[SchemeValue]): SchemeValue = proc match
    case SLambda(params, body, closure) =>
      if params.length != args.length then
        throw new EvalError(s"expected ${params.length} arguments, got ${args.length}")
      val localEnv = closure.extend(params, args)
      body.map(e => eval(e, localEnv)).last
    case SBuiltin(_, fn) => fn(args)
    case other           => throw new EvalError(s"not a procedure: ${other.display}")

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => quotify(single)
      case _             => throw new EvalError("quote: requires exactly 1 argument")

  private def quotify(v: SchemeValue): SchemeValue = v match
    case SList(elems) => elems.foldRight(SNil: SchemeValue)((e, acc) => SPair(quotify(e), acc))
    case other        => other

  private def evalIf(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if isTruthy(eval(cond, env)) then eval(thenBranch, env) else eval(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if isTruthy(eval(cond, env)) then eval(thenBranch, env) else SVoid
      case _ => throw new EvalError("if: bad syntax")

  private def evalDefine(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case SSymbol(name) :: value :: Nil =>
        env.define(name, eval(value, env))
        SVoid
      case SList(SSymbol(name) :: params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SSymbol(n) => n
          case other      => throw new EvalError(s"define: expected parameter name, got ${other.display}")
        }
        env.define(name, SLambda(paramNames, body, env))
        SVoid
      case _ => throw new EvalError("define: bad syntax")

  private def evalLambda(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case SList(params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SSymbol(n) => n
          case other      => throw new EvalError(s"lambda: expected parameter name, got ${other.display}")
        }
        SLambda(paramNames, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def asInteger(v: SchemeValue): Long = v match
    case SInteger(n) => n
    case other       => throw new EvalError(s"expected number, got ${other.display}")

  private def evalAdd(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(asInteger).sum)

  private def evalSub(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil           => throw new EvalError("-: requires at least 1 argument")
      case single :: Nil => SInteger(-asInteger(single))
      case first :: rest =>
        val firstVal = asInteger(first)
        SInteger(rest.foldLeft(firstVal)((acc, a) => acc - asInteger(a)))

  private def evalMul(args: List[SchemeValue]): SchemeValue =
    SInteger(args.map(asInteger).product)

  private def evalDiv(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case single :: Nil =>
        val v = asInteger(single)
        if v == 0 then throw new EvalError("division by zero")
        SInteger(1 / v)
      case first :: rest =>
        val firstVal = asInteger(first)
        SInteger(rest.foldLeft(firstVal) { (acc, a) =>
          val v = asInteger(a)
          if v == 0 then throw new EvalError("division by zero")
          acc / v
        })

  private def evalCompare(args: List[SchemeValue], cmp: (Long, Long) => Boolean): SchemeValue =
    val vals = args.map(asInteger)
    SBoolean(vals.zip(vals.tail).forall((a, b) => cmp(a, b)))

  private def isTruthy(v: SchemeValue): Boolean = v match
    case SBoolean(false) => false
    case _               => true

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SBoolean(!isTruthy(single))
      case _             => throw new EvalError(s"not: requires exactly 1 argument")

  private def evalAnd(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil         => SBoolean(true)
      case last :: Nil => eval(last, env)
      case head :: rest =>
        val v = eval(head, env)
        if !isTruthy(v) then v else evalAnd(rest, env)

  private def evalOr(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil         => SBoolean(false)
      case last :: Nil => eval(last, env)
      case head :: rest =>
        val v = eval(head, env)
        if isTruthy(v) then v else evalOr(rest, env)

  private def evalLet(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case SSymbol(name) :: SList(bindings) :: body if body.nonEmpty =>
        val (paramNames, initVals) = bindings.map {
          case SList(SSymbol(p) :: expr :: Nil) => (p, eval(expr, env))
          case other                            => throw new EvalError(s"let: bad binding: ${other.display}")
        }.unzip
        val localEnv = env.extend(Nil, Nil)
        val lambda   = SLambda(paramNames, body, localEnv)
        localEnv.define(name, lambda)
        applyProc(lambda, initVals)
      case SList(bindings) :: body if body.nonEmpty =>
        val (names, vals) = bindings.map {
          case SList(SSymbol(name) :: expr :: Nil) => (name, eval(expr, env))
          case other                               => throw new EvalError(s"let: bad binding: ${other.display}")
        }.unzip
        val localEnv = env.extend(names, vals)
        body.map(e => eval(e, localEnv)).last
      case _ => throw new EvalError("let: bad syntax")

  private def evalBegin(args: List[SchemeValue], env: Environment): SchemeValue =
    if args.isEmpty then SVoid
    else args.map(e => eval(e, env)).last

  private def evalCond(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil => SVoid
      case SList(SSymbol("else") :: body) :: Nil =>
        if body.isEmpty then throw new EvalError("cond: else clause must have body")
        body.map(e => eval(e, env)).last
      case SList(test :: body) :: rest =>
        if isTruthy(eval(test, env)) then
          if body.isEmpty then eval(test, env)
          else body.map(e => eval(e, env)).last
        else evalCond(rest, env)
      case _ => throw new EvalError("cond: bad syntax")

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => SPair(a, b)
      case _             => throw new EvalError("cons: requires exactly 2 arguments")

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    args match
      case SPair(a, _) :: Nil => a
      case _                  => throw new EvalError("car: requires a pair")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    args match
      case SPair(_, d) :: Nil => d
      case _                  => throw new EvalError("cdr: requires a pair")

  private def evalNullPred(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SBoolean(single == SNil)
      case _             => throw new EvalError("null?: requires exactly 1 argument")

  private def evalListBuiltin(args: List[SchemeValue]): SchemeValue =
    args.foldRight(SNil: SchemeValue)((e, acc) => SPair(e, acc))

  private def evalLength(args: List[SchemeValue]): SchemeValue =
    args match
      case single :: Nil => SInteger(listLength(single))
      case _             => throw new EvalError("length: requires exactly 1 argument")

  @scala.annotation.tailrec
  private def listLength(v: SchemeValue, acc: Long = 0): Long = v match
    case SNil        => acc
    case SPair(_, d) => listLength(d, acc + 1)
    case _           => throw new EvalError("length: not a proper list")

  private def evalAppend(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil         => SNil
      case last :: Nil => last
      case head :: rest =>
        val restResult = evalAppend(rest)
        appendTwo(head, restResult)

  private def appendTwo(a: SchemeValue, b: SchemeValue): SchemeValue = a match
    case SNil        => b
    case SPair(h, t) => SPair(h, appendTwo(t, b))
    case _           => throw new EvalError("append: not a proper list")

  private def typeCheck(args: List[SchemeValue], pred: SchemeValue => Boolean): SchemeValue =
    args match
      case single :: Nil => SBoolean(pred(single))
      case _             => throw new EvalError("type predicate: requires exactly 1 argument")
