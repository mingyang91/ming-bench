package ming

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env               = makeGlobalEnv()
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    result.display

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output            = new StringBuilder
    val env               = makeGlobalEnv(output)
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(mutable.Map.empty, None)
    Builtins.install(env, output)
    env

  private def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private def eval(expr: Expr, env: Env): SchemeVal =
    try
      expr match
        case IntLit(v, _)    => SchemeInt(v)
        case BoolLit(v, _)   => SchemeBool(v)
        case StringLit(v, _) => SchemeString(v)
        case Symbol(name, _) => env.get(name)
        case SList(Nil, _)   => throw new EvalError("empty application")
        case SList(elems, _) => evalApplication(elems, env)
    catch
      case e: EvalError =>
        val msg = e.getMessage
        if msg.matches(".*\\d+:\\d+.*") then throw e
        else throw new EvalError(s"${expr.pos}: $msg")

  private def evalApplication(elems: List[Expr], env: Env): SchemeVal =
    elems.head match
      case Symbol("and", _)    => evalAnd(elems.tail, env)
      case Symbol("or", _)     => evalOr(elems.tail, env)
      case Symbol("define", _) => evalDefine(elems.tail, env)
      case Symbol("if", _)     => evalIf(elems.tail, env)
      case Symbol("quote", _)  => evalQuote(elems.tail)
      case Symbol("lambda", _) => evalLambda(elems.tail, env)
      case Symbol("begin", _)  => evalBegin(elems.tail, env)
      case Symbol("let", _)    => evalLet(elems.tail, env)
      case Symbol("cond", _)   => evalCond(elems.tail, env)
      case _ =>
        val op   = eval(elems.head, env)
        val args = elems.tail.map(e => eval(e, env))
        applyProc(op, args)

  private def evalAnd(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then return SchemeBool(true)
    var result: SchemeVal = SchemeBool(true)
    val iter              = exprs.iterator
    var done              = false
    while iter.hasNext && !done do
      result = eval(iter.next(), env)
      if isFalsy(result) then done = true
    result

  private def evalOr(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then return SchemeBool(false)
    var result: SchemeVal = SchemeBool(false)
    val iter              = exprs.iterator
    var done              = false
    while iter.hasNext && !done do
      result = eval(iter.next(), env)
      if !isFalsy(result) then done = true
    result

  private def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeBuiltin(_, fn) => fn(args)
      case SchemeLambda(params, body, closureEnv) =>
        if params.size != args.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
        val localEnv          = new Env(mutable.Map.from(params.zip(args)), Some(closureEnv))
        var result: SchemeVal = SchemeVoid
        for expr <- body do result = eval(expr, localEnv)
        result
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private def evalDefine(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(Symbol(name, _) :: params, _) :: body =>
        val paramNames = params.map {
          case Symbol(n, _) => n
          case other        => throw new EvalError("define: expected parameter name")
        }
        env.set(name, SchemeLambda(paramNames, body, env))
        SchemeVoid
      case Symbol(name, _) :: expr :: Nil =>
        env.set(name, eval(expr, env))
        SchemeVoid
      case _ => throw new EvalError("define: bad syntax")

  private def evalIf(args: List[Expr], env: Env): SchemeVal =
    args match
      case cond :: thenExpr :: elseExpr :: Nil =>
        if !isFalsy(eval(cond, env)) then eval(thenExpr, env) else eval(elseExpr, env)
      case cond :: thenExpr :: Nil =>
        if !isFalsy(eval(cond, env)) then eval(thenExpr, env) else SchemeVoid
      case _ => throw new EvalError("if: bad syntax")

  private def evalQuote(args: List[Expr]): SchemeVal =
    if args.size != 1 then throw new EvalError("quote: expected 1 argument")
    exprToVal(args.head)

  private def exprToVal(expr: Expr): SchemeVal =
    expr match
      case IntLit(v, _)    => SchemeInt(v)
      case BoolLit(v, _)   => SchemeBool(v)
      case StringLit(v, _) => SchemeString(v)
      case Symbol(name, _) => SchemeSymbol(name)
      case SList(elems, _) => SchemeList(elems.map(exprToVal))

  private def evalLambda(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(paramExprs, _) :: body if body.nonEmpty =>
        val params = paramExprs.map {
          case Symbol(n, _) => n
          case _            => throw new EvalError("lambda: expected parameter name")
        }
        SchemeLambda(params, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalBegin(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else
      var result: SchemeVal = SchemeVoid
      for expr <- exprs do result = eval(expr, env)
      result

  private def evalLet(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        val parsed   = parseBindings(bindings, env)
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val lambda   = SchemeLambda(parsed.map(_._1), body, localEnv)
        localEnv.set(name, lambda)
        applyProc(lambda, parsed.map(_._2))
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
              localEnv.set(n, eval(initExpr, env))
            case _ => throw new EvalError("let: bad binding")
        var result: SchemeVal = SchemeVoid
        for expr <- body do result = eval(expr, localEnv)
        result
      case _ => throw new EvalError("let: bad syntax")

  private def parseBindings(
    bindings: List[Expr],
    env: Env
  ): List[(String, SchemeVal)] =
    bindings.map {
      case SList(Symbol(n, _) :: initExpr :: Nil, _) => (n, eval(initExpr, env))
      case _                                         => throw new EvalError("let: bad binding")
    }

  private def evalCond(clauses: List[Expr], env: Env): SchemeVal =
    boundary:
      for clause <- clauses do
        clause match
          case SList(Symbol("else", _) :: body, _) =>
            break(evalBody(body, env))
          case SList(test :: body, _) =>
            val testVal = eval(test, env)
            if !isFalsy(testVal) then break(if body.isEmpty then testVal else evalBody(body, env))
          case _ => throw new EvalError("cond: bad clause")
      SchemeVoid

  private def evalBody(exprs: List[Expr], env: Env): SchemeVal =
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    result
