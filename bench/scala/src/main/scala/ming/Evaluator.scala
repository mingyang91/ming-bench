package ming

import Value.*
import Expr.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    evalSequence(exprs, env).display

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def makeGlobalEnv(): Env =
    val env = Env()
    val builtins: List[(String, List[Value] => Value)] = List(
      ("+", args => arith(args, 0L, _ + _)),
      ("*", args => arith(args, 1L, _ * _)),
      ("-", args => subtractOp(args)),
      ("/", args => divideOp(args)),
      ("<", args => compareOp(args, _ < _)),
      (">", args => compareOp(args, _ > _)),
      ("=", args => compareOp(args, _ == _)),
      ("<=", args => compareOp(args, _ <= _)),
      (">=", args => compareOp(args, _ >= _)),
      (
        "not",
        args =>
          if args.length != 1 then throw new EvalError("not: expected 1 argument")
          BoolVal(!args.head.isTruthy)
      )
    )
    builtins.foreach((name, fn) => env.define(name, BuiltinVal(name, fn)))
    env

  private def evalSequence(exprs: List[Expr], env: Env): Value =
    exprs match
      case Nil         => throw new EvalError("empty sequence")
      case last :: Nil => eval(last, env)
      case head :: tail =>
        eval(head, env)
        evalSequence(tail, env)

  private def eval(expr: Expr, env: Env): Value = expr match
    case Num(n)                   => IntVal(n)
    case Bool(b)                  => BoolVal(b)
    case Str(s)                   => StrVal(s)
    case Sym(name)                => env.lookup(name)
    case SList(Nil)               => throw new EvalError("empty application")
    case SList(Sym("if") :: args) => evalIf(args, env)
    case SList(Sym("define") :: args) =>
      evalDefine(args, env)
      BoolVal(true)
    case SList(Sym("quote") :: args)  => evalQuote(args)
    case SList(Sym("lambda") :: args) => evalLambda(args, env)
    case SList(Sym("and") :: args)    => evalAnd(args, env)
    case SList(Sym("or") :: args)     => evalOr(args, env)
    case SList(Sym("not") :: args)    => evalNot(args, env)
    case SList(head :: args) =>
      val proc   = eval(head, env)
      val values = args.map(eval(_, env))
      applyProc(proc, values)

  private def evalIf(args: List[Expr], env: Env): Value =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBranch, env)
        else eval(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBranch, env)
        else BoolVal(false)
      case _ => throw new EvalError("if: bad syntax")

  private def evalDefine(args: List[Expr], env: Env): Unit =
    args match
      case Sym(name) :: valueExpr :: Nil =>
        val v = eval(valueExpr, env)
        env.define(name, v)
      case SList(Sym(name) :: params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p) => p
          case _      => throw new EvalError("define: non-symbol parameter")
        }
        val lambda = LambdaVal(paramNames, body, env)
        env.define(name, lambda)
      case _ => throw new EvalError("define: bad syntax")

  private def evalQuote(args: List[Expr]): Value =
    args match
      case expr :: Nil => exprToValue(expr)
      case _           => throw new EvalError("quote: expected 1 argument")

  private def exprToValue(expr: Expr): Value = expr match
    case Num(n)  => IntVal(n)
    case Bool(b) => BoolVal(b)
    case Str(s)  => StrVal(s)
    case Sym(s)  => SymbolVal(s)
    case SList(elems) =>
      elems.foldRight(NilVal: Value)((e, acc) => PairVal(exprToValue(e), acc))

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case SList(params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p) => p
          case _      => throw new EvalError("lambda: non-symbol parameter")
        }
        LambdaVal(paramNames, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalAnd(args: List[Expr], env: Env): Value =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if !v.isTruthy then v else evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Value =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if v.isTruthy then v else evalOr(tail, env)

  private def evalNot(args: List[Expr], env: Env): Value =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    BoolVal(!eval(args.head, env).isTruthy)

  private def applyProc(proc: Value, args: List[Value]): Value =
    proc match
      case LambdaVal(params, body, closure) =>
        val localEnv = closure.extend(params, args)
        evalSequence(body, localEnv)
      case BuiltinVal(_, fn) => fn(args)
      case _                 => throw new EvalError("not a procedure")

  private def applyPrimitive(op: String, args: List[Value]): Value =
    op match
      case "+"  => arith(args, 0L, _ + _)
      case "*"  => arith(args, 1L, _ * _)
      case "-"  => subtractOp(args)
      case "/"  => divideOp(args)
      case "<"  => compareOp(args, _ < _)
      case ">"  => compareOp(args, _ > _)
      case "="  => compareOp(args, _ == _)
      case "<=" => compareOp(args, _ <= _)
      case ">=" => compareOp(args, _ >= _)
      case _    => throw new EvalError(s"unknown procedure: $op")

  private def requireInts(args: List[Value]): List[Long] =
    args.map {
      case IntVal(n) => n
      case other     => throw new EvalError(s"expected number, got ${other.display}")
    }

  private def arith(args: List[Value], identity: Long, op: (Long, Long) => Long): Value =
    val nums = requireInts(args)
    IntVal(nums.foldLeft(identity)(op))

  private def subtractOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => IntVal(0)
      case n :: Nil  => IntVal(-n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ - _))

  private def divideOp(args: List[Value]): Value =
    val nums = requireInts(args)
    nums match
      case Nil       => throw new EvalError("/: need at least 1 argument")
      case n :: Nil  => IntVal(1 / n)
      case n :: rest => IntVal(rest.foldLeft(n)(_ / _))

  private def compareOp(args: List[Value], op: (Long, Long) => Boolean): Value =
    val nums   = requireInts(args)
    val result = nums.zip(nums.tail).forall((a, b) => op(a, b))
    BoolVal(result)
