package ming

import scala.collection.mutable

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
    throw new EvalError("not implemented")

  private def asLong(v: SchemeVal, op: String): Long = v match
    case SchemeInt(n) => n
    case _            => throw new EvalError(s"$op: expected number, got ${v.display}")

  private def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)

    env.set("+", SchemeBuiltin("+", args => SchemeInt(args.map(a => asLong(a, "+")).sum)))
    env.set(
      "-",
      SchemeBuiltin(
        "-",
        args =>
          if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if args.size == 1 then SchemeInt(-asLong(args.head, "-"))
          else SchemeInt(args.map(a => asLong(a, "-")).reduce(_ - _))
      )
    )
    env.set("*", SchemeBuiltin("*", args => SchemeInt(args.map(a => asLong(a, "*")).product)))
    env.set(
      "/",
      SchemeBuiltin(
        "/",
        args =>
          if args.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          else
            val nums = args.map(a => asLong(a, "/"))
            if nums.tail.exists(_ == 0) then throw new EvalError("/: division by zero")
            SchemeInt(nums.reduce(_ / _))
      )
    )

    def cmp(name: String, op: (Long, Long) => Boolean): SchemeBuiltin =
      SchemeBuiltin(
        name,
        args =>
          if args.size < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
          val nums = args.map(a => asLong(a, name))
          SchemeBool(nums.sliding(2).forall(w => op(w(0), w(1))))
      )

    env.set("<", cmp("<", _ < _))
    env.set(">", cmp(">", _ > _))
    env.set("=", cmp("=", _ == _))
    env.set("<=", cmp("<=", _ <= _))
    env.set(">=", cmp(">=", _ >= _))

    env.set(
      "not",
      SchemeBuiltin(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          SchemeBool(isFalsy(args.head))
      )
    )

    env

  private def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private def eval(expr: Expr, env: Env): SchemeVal =
    expr match
      case IntLit(v)    => SchemeInt(v)
      case BoolLit(v)   => SchemeBool(v)
      case StringLit(v) => SchemeString(v)
      case Symbol(name) => env.get(name)
      case SList(Nil)   => throw new EvalError("empty application")
      case SList(elems) => evalApplication(elems, env)

  private def evalApplication(elems: List[Expr], env: Env): SchemeVal =
    elems.head match
      case Symbol("and")    => evalAnd(elems.tail, env)
      case Symbol("or")     => evalOr(elems.tail, env)
      case Symbol("define") => evalDefine(elems.tail, env)
      case Symbol("if")     => evalIf(elems.tail, env)
      case Symbol("quote")  => evalQuote(elems.tail)
      case Symbol("lambda") => evalLambda(elems.tail, env)
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
      case SList(Symbol(name) :: params) :: body =>
        val paramNames = params.map {
          case Symbol(n) => n
          case other     => throw new EvalError("define: expected parameter name")
        }
        env.set(name, SchemeLambda(paramNames, body, env))
        SchemeVoid
      case Symbol(name) :: expr :: Nil =>
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
      case IntLit(v)    => SchemeInt(v)
      case BoolLit(v)   => SchemeBool(v)
      case StringLit(v) => SchemeString(v)
      case Symbol(name) => SchemeSymbol(name)
      case SList(elems) => SchemeList(elems.map(exprToVal))

  private def evalLambda(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(paramExprs) :: body if body.nonEmpty =>
        val params = paramExprs.map {
          case Symbol(n) => n
          case _         => throw new EvalError("lambda: expected parameter name")
        }
        SchemeLambda(params, body, env)
      case _ => throw new EvalError("lambda: bad syntax")
