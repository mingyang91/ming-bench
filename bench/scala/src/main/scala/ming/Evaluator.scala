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
    evalBody(exprs, env).display

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output = new StringBuilder
    val env    = makeGlobalEnv(output)
    val result = evalBody(exprs, env)
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = Env()
    Builtins.register(env, output)
    env

  /** Evaluate a sequence of expressions, returning the last value. Uses TCO for the last expr. */
  private def evalBody(exprs: List[Expr], env: Env): Value =
    exprs match
      case Nil         => throw new EvalError("empty sequence")
      case last :: Nil => eval(last, env)
      case head :: tail =>
        eval(head, env)
        evalBody(tail, env)

  /** Core evaluator with trampoline loop for tail-call optimization. */
  private def eval(expr0: Expr, env0: Env): Value =
    var curExpr: Expr = expr0
    var curEnv: Env   = env0

    while true do
      curExpr match
        case Num(n, _)       => return IntVal(n)
        case Bool(b, _)      => return BoolVal(b)
        case Str(s, _)       => return StrVal(s.toCharArray)
        case Chr(c, _)       => return CharVal(c)
        case Sym(name, pos)  => return curEnv.lookup(name, pos)
        case SList(Nil, pos) => evalError("empty application", pos)

        case SList(Sym("if", _) :: args, pos) =>
          args match
            case cond :: thenBranch :: elseBranch :: Nil =>
              if eval(cond, curEnv).isTruthy then curExpr = thenBranch
              else curExpr = elseBranch
            case cond :: thenBranch :: Nil =>
              if eval(cond, curEnv).isTruthy then curExpr = thenBranch
              else return BoolVal(false)
            case _ => evalError("if: bad syntax", pos)

        case SList(Sym("define", _) :: args, pos) =>
          evalDefine(args, curEnv, pos)
          return BoolVal(true)

        case SList(Sym("set!", _) :: args, pos) =>
          args match
            case Sym(name, _) :: valueExpr :: Nil =>
              val v = eval(valueExpr, curEnv)
              curEnv.set(name, v, pos)
              return BoolVal(true)
            case _ => evalError("set!: bad syntax", pos)

        case SList(Sym("quote", _) :: args, pos) =>
          return evalQuote(args, pos)

        case SList(Sym("lambda", _) :: args, pos) =>
          return evalLambda(args, curEnv, pos)

        case SList(Sym("let", _) :: args, pos) =>
          val (nextExpr, nextEnv) = evalLetTco(args, curEnv, pos)
          curExpr = nextExpr
          curEnv = nextEnv

        case SList(Sym("begin", _) :: args, pos) =>
          if args.isEmpty then evalError("begin: empty", pos)
          args.init.foreach(eval(_, curEnv))
          curExpr = args.last

        case SList(Sym("cond", _) :: clauses, pos) =>
          evalCondTco(clauses, curEnv, pos) match
            case Left(value) => return value
            case Right((expr, env)) =>
              curExpr = expr
              curEnv = env

        case SList(Sym("and", _) :: args, _) =>
          args match
            case Nil => return BoolVal(true)
            case _ =>
              var remaining = args
              while remaining.tail.nonEmpty do
                val v = eval(remaining.head, curEnv)
                if !v.isTruthy then return v
                remaining = remaining.tail
              curExpr = remaining.head

        case SList(Sym("or", _) :: args, _) =>
          args match
            case Nil => return BoolVal(false)
            case _ =>
              var remaining = args
              while remaining.tail.nonEmpty do
                val v = eval(remaining.head, curEnv)
                if v.isTruthy then return v
                remaining = remaining.tail
              curExpr = remaining.head

        case SList(Sym("not", _) :: args, _) =>
          if args.length != 1 then throw new EvalError("not: expected 1 argument")
          return BoolVal(!eval(args.head, curEnv).isTruthy)

        case SList(head :: args, pos) =>
          applyProc(eval(head, curEnv), args.map(eval(_, curEnv)), pos) match
            case Left(value) => return value
            case Right((expr, env)) =>
              curExpr = expr
              curEnv = env
    end while
    throw new AssertionError("unreachable")

  /** Apply a procedure value to arguments. Returns Left for immediate result, Right for tail-call continuation. */
  private def applyProc(proc: Value, values: List[Value], pos: Option[Pos]): Either[Value, (Expr, Env)] =
    proc match
      case LambdaVal(params, body, closure) =>
        val localEnv = closure.extend(params, values)
        body.init.foreach(eval(_, localEnv))
        Right((body.last, localEnv))
      case BuiltinVal(name, fn) =>
        try Left(fn(values))
        catch
          case e: EvalError =>
            if e.getMessage.matches(".*\\d+:\\d+.*") then throw e
            else evalError(e.getMessage, pos)
          case e: ArithmeticException =>
            evalError(e.getMessage, pos)
      case _ => evalError("not a procedure", pos)

  /** Find matching cond clause. Returns Left(value) for immediate result, or Right((expr, env)) for tail-position
    * continuation.
    */
  private def evalCondTco(
    clauses: List[Expr],
    env: Env,
    pos: Option[Pos]
  ): Either[Value, (Expr, Env)] =
    clauses match
      case Nil => evalError("cond: no matching clause", pos)
      case SList(Sym("else", _) :: body, _) :: Nil =>
        body.init.foreach(eval(_, env))
        Right((body.last, env))
      case SList(test :: body, _) :: rest =>
        if eval(test, env).isTruthy then
          body.init.foreach(eval(_, env))
          Right((body.last, env))
        else evalCondTco(rest, env, pos)
      case _ => evalError("cond: bad syntax", pos)

  private def evalLetTco(
    args: List[Expr],
    env: Env,
    pos: Option[Pos]
  ): (Expr, Env) =
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
        val callEnv = localEnv.extend(paramNames, initVals)
        body.init.foreach(eval(_, callEnv))
        (body.last, callEnv)

      case SList(bindings, _) :: body if body.nonEmpty =>
        val pairs = bindings.map {
          case SList(Sym(v, _) :: valExpr :: Nil, _) => (v, eval(valExpr, env))
          case _                                     => evalError("let: bad binding", pos)
        }
        val localEnv = env.extend(pairs.map(_._1), pairs.map(_._2))
        body.init.foreach(eval(_, localEnv))
        (body.last, localEnv)

      case _ => evalError("let: bad syntax", pos)

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
    case Str(s, _)  => StrVal(s.toCharArray)
    case Chr(c, _)  => CharVal(c)
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
