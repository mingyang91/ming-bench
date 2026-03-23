package ming

import Value.*
import Expr.*

/** Scheme interpreter entry point. */
object Evaluator:

  private def evalError(msg: String, pos: Option[Pos]): Nothing =
    pos match
      case Some(p) => throw new EvalError(s"$msg [$p]")
      case None    => throw new EvalError(msg)

  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    evalSequence(exprs, env).display

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
      ),
      (
        "cons",
        args =>
          if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
          PairVal(args(0), args(1))
      ),
      (
        "car",
        args =>
          if args.length != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
            case PairVal(car, _) => car
            case _               => throw new EvalError("car: not a pair")
      ),
      (
        "cdr",
        args =>
          if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
          args.head match
            case PairVal(_, cdr) => cdr
            case _               => throw new EvalError("cdr: not a pair")
      ),
      (
        "null?",
        args =>
          if args.length != 1 then throw new EvalError("null?: expected 1 argument")
          BoolVal(args.head == NilVal)
      ),
      (
        "list",
        args => args.foldRight(NilVal: Value)((v, acc) => PairVal(v, acc))
      ),
      (
        "length",
        args =>
          if args.length != 1 then throw new EvalError("length: expected 1 argument")
          var count = 0L
          var cur   = args.head
          while cur match
              case PairVal(_, cdr) => count += 1; cur = cdr; true
              case NilVal          => false
              case _               => throw new EvalError("length: not a proper list")
          do ()
          IntVal(count)
      ),
      (
        "append",
        args =>
          args.foldRight(NilVal: Value) { (lst, acc) =>
            appendList(lst, acc)
          }
      ),
      ("string?", args => typePred(args, _.isInstanceOf[StrVal])),
      ("number?", args => typePred(args, _.isInstanceOf[IntVal])),
      ("boolean?", args => typePred(args, _.isInstanceOf[BoolVal])),
      ("pair?", args => typePred(args, _.isInstanceOf[PairVal])),
      ("symbol?", args => typePred(args, _.isInstanceOf[SymbolVal]))
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
    case Num(n, _)                            => IntVal(n)
    case Bool(b, _)                           => BoolVal(b)
    case Str(s, _)                            => StrVal(s)
    case Sym(name, pos)                       => env.lookup(name, pos)
    case SList(Nil, pos)                      => evalError("empty application", pos)
    case SList(Sym("if", _) :: args, pos)     => evalIf(args, env, pos)
    case SList(Sym("define", _) :: args, pos) => evalDefine(args, env, pos); BoolVal(true)
    case SList(Sym("quote", _) :: args, pos)  => evalQuote(args, pos)
    case SList(Sym("lambda", _) :: args, pos) => evalLambda(args, env, pos)
    case SList(Sym("let", _) :: args, pos)    => evalLet(args, env, pos)
    case SList(Sym("begin", _) :: args, pos)  => evalBegin(args, env, pos)
    case SList(Sym("cond", _) :: args, pos)   => evalCond(args, env, pos)
    case SList(Sym("and", _) :: args, _)      => evalAnd(args, env)
    case SList(Sym("or", _) :: args, _)       => evalOr(args, env)
    case SList(Sym("not", _) :: args, _)      => evalNot(args, env)
    case SList(head :: args, pos) =>
      val proc   = eval(head, env)
      val values = args.map(eval(_, env))
      applyProc(proc, values, pos)

  private def evalIf(args: List[Expr], env: Env, pos: Option[Pos]): Value =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBranch, env)
        else eval(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBranch, env)
        else BoolVal(false)
      case _ => evalError("if: bad syntax", pos)

  private def evalDefine(args: List[Expr], env: Env, pos: Option[Pos]): Unit =
    args match
      case Sym(name, _) :: valueExpr :: Nil =>
        val v = eval(valueExpr, env)
        env.define(name, v)
      case SList(Sym(name, _) :: params, _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p, _) => p
          case _         => evalError("define: non-symbol parameter", pos)
        }
        val lambda = LambdaVal(paramNames, body, env)
        env.define(name, lambda)
      case _ => evalError("define: bad syntax", pos)

  private def evalQuote(args: List[Expr], pos: Option[Pos]): Value =
    args match
      case expr :: Nil => exprToValue(expr)
      case _           => evalError("quote: expected 1 argument", pos)

  private def exprToValue(expr: Expr): Value = expr match
    case Num(n, _)  => IntVal(n)
    case Bool(b, _) => BoolVal(b)
    case Str(s, _)  => StrVal(s)
    case Sym(s, _)  => SymbolVal(s)
    case SList(elems, _) =>
      elems.foldRight(NilVal: Value)((e, acc) => PairVal(exprToValue(e), acc))

  private def evalLambda(args: List[Expr], env: Env, pos: Option[Pos]): Value =
    args match
      case SList(params, _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case Sym(p, _) => p
          case _         => evalError("lambda: non-symbol parameter", pos)
        }
        LambdaVal(paramNames, body, env)
      case _ => evalError("lambda: bad syntax", pos)

  private def evalLet(args: List[Expr], env: Env, pos: Option[Pos]): Value =
    args match
      case Sym(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        val pairs = bindings.map {
          case SList(Sym(v, _) :: valExpr :: Nil, _) => (v, eval(valExpr, env))
          case _                                     => evalError("let: bad binding", pos)
        }
        val paramNames = pairs.map(_._1)
        val initVals   = pairs.map(_._2)
        val localEnv   = env.extend(Nil, Nil)
        val lambda     = LambdaVal(paramNames, body, localEnv)
        localEnv.define(name, lambda)
        applyProc(lambda, initVals, pos)
      case SList(bindings, _) :: body if body.nonEmpty =>
        val pairs = bindings.map {
          case SList(Sym(v, _) :: valExpr :: Nil, _) => (v, eval(valExpr, env))
          case _                                     => evalError("let: bad binding", pos)
        }
        val localEnv = env.extend(pairs.map(_._1), pairs.map(_._2))
        evalSequence(body, localEnv)
      case _ => evalError("let: bad syntax", pos)

  private def evalBegin(args: List[Expr], env: Env, pos: Option[Pos]): Value =
    if args.isEmpty then evalError("begin: empty", pos)
    evalSequence(args, env)

  private def evalCond(clauses: List[Expr], env: Env, pos: Option[Pos]): Value =
    clauses match
      case Nil => evalError("cond: no matching clause", pos)
      case SList(Sym("else", _) :: body, _) :: Nil =>
        evalSequence(body, env)
      case SList(test :: body, _) :: rest =>
        if eval(test, env).isTruthy then evalSequence(body, env)
        else evalCond(rest, env, pos)
      case _ => evalError("cond: bad syntax", pos)

  private def appendList(lst: Value, tail: Value): Value =
    lst match
      case NilVal        => tail
      case PairVal(h, t) => PairVal(h, appendList(t, tail))
      case _             => throw new EvalError("append: not a proper list")

  private def typePred(args: List[Value], pred: Value => Boolean): Value =
    if args.length != 1 then throw new EvalError("type predicate: expected 1 argument")
    BoolVal(pred(args.head))

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

  private def applyProc(proc: Value, args: List[Value], pos: Option[Pos] = None): Value =
    proc match
      case LambdaVal(params, body, closure) =>
        val localEnv = closure.extend(params, args)
        evalSequence(body, localEnv)
      case BuiltinVal(name, fn) =>
        try fn(args)
        catch
          case e: EvalError =>
            if e.getMessage.matches(".*\\d+:\\d+.*") then throw e
            else evalError(e.getMessage, pos)
          case e: ArithmeticException =>
            evalError(e.getMessage, pos)
      case _ => evalError("not a procedure", pos)

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
