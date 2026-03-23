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
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output = new StringBuilder
    val env    = makeGlobalEnv(output)
    val result = evalSequence(exprs, env)
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = Env()
    Builtins.register(env, output)
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
