package ming

import SchemeModel.*

object SchemeInterpreter:

  def evalToString(input: String): String =
    render(evalProgram(input))

  def evalToStringWithOutput(input: String): (String, String) =
    (render(evalProgram(input)), "")

  private def evalProgram(input: String): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("empty program")
    evalSequence(expressions, baseEnv())

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => env.lookup(name)
      case Expr.ListExpr(Nil) =>
        throw new EvalError("cannot evaluate an empty list")
      case Expr.ListExpr(Expr.Symbol("quote") :: args) =>
        evalQuote(args)
      case Expr.ListExpr(Expr.Symbol("if") :: args) =>
        evalIf(args, env)
      case Expr.ListExpr(Expr.Symbol("define") :: args) =>
        evalDefine(args, env)
      case Expr.ListExpr(Expr.Symbol("lambda") :: args) =>
        evalLambda(args, env)
      case Expr.ListExpr(Expr.Symbol("and") :: rest) =>
        evalAnd(rest, env)
      case Expr.ListExpr(Expr.Symbol("or") :: rest) =>
        evalOr(rest, env)
      case Expr.ListExpr(operator :: args) =>
        apply(eval(operator, env), args.map(eval(_, env)))

  private def evalQuote(args: List[Expr]): Value =
    args match
      case expr :: Nil => quoteExpr(expr)
      case _ =>
        throw new EvalError(s"quote expected 1 argument, got ${args.length}")

  private def evalIf(args: List[Expr], env: Env): Value =
    args match
      case condition :: whenTrue :: whenFalse :: Nil =>
        if isTruthy(eval(condition, env)) then eval(whenTrue, env)
        else eval(whenFalse, env)
      case _ =>
        throw new EvalError(s"if expected 3 arguments, got ${args.length}")

  private def evalDefine(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.VoidValue
      case Expr.ListExpr(Expr.Symbol(name) :: params) :: body if body.nonEmpty =>
        env.define(name, buildClosure(params, body, env, Some(name)))
        Value.VoidValue
      case _ =>
        throw new EvalError("invalid define form")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(params) :: body if body.nonEmpty =>
        buildClosure(params, body, env, None)
      case _ =>
        throw new EvalError("invalid lambda form")

  private def buildClosure(
    paramsExpr: List[Expr],
    body: List[Expr],
    env: Env,
    name: Option[String]
  ): Value =
    val params = paramsExpr.map {
      case Expr.Symbol(paramName) => paramName
      case _                      => throw new EvalError("lambda parameters must be symbols")
    }
    if params.distinct.length != params.length then throw new EvalError("lambda parameters must be distinct")
    Value.Closure(name, params, body, env)

  private def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => Value.SymbolValue(name)
      case Expr.ListExpr(items) =>
        items.foldRight(Value.NilValue: Value) { (item, rest) =>
          Value.PairValue(quoteExpr(item), rest)
        }

  private def evalAnd(args: List[Expr], env: Env): Value =
    var result: Value = Value.BooleanValue(true)
    val iterator      = args.iterator
    while iterator.hasNext do
      result = eval(iterator.next(), env)
      if !isTruthy(result) then return result
    result

  private def evalOr(args: List[Expr], env: Env): Value =
    var result: Value = Value.BooleanValue(false)
    val iterator      = args.iterator
    while iterator.hasNext do
      result = eval(iterator.next(), env)
      if isTruthy(result) then return result
    result

  private def apply(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case closure: Value.Closure           => applyClosure(closure, args)
      case other =>
        throw new EvalError(s"not a procedure: ${render(other)}")

  private def applyClosure(closure: Value.Closure, args: List[Value]): Value =
    requireArgCount(closure.name.getOrElse("lambda"), args, closure.params.length)
    val callEnv = new Env(Some(closure.env))
    closure.params.zip(args).foreach { case (name, value) =>
      callEnv.define(name, value)
    }
    evalSequence(closure.body, callEnv)

  private def evalSequence(expressions: List[Expr], env: Env): Value =
    var result: Value = Value.VoidValue
    expressions.foreach(expr => result = eval(expr, env))
    result

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  private def render(value: Value): String =
    value match
      case Value.IntegerValue(number) => number.toString
      case Value.BooleanValue(flag)   => if flag then "#t" else "#f"
      case Value.StringValue(text)    => s""""${escapeString(text)}""""
      case Value.SymbolValue(name)    => name
      case Value.NilValue             => "()"
      case pair: Value.PairValue      => renderPair(pair)
      case Value.Builtin(name, _)     => s"#<procedure:$name>"
      case Value.Closure(Some(name), _, _, _) =>
        s"#<procedure:$name>"
      case Value.Closure(None, _, _, _) =>
        "#<procedure>"
      case Value.VoidValue =>
        "#<void>"

  private def renderPair(pair: Value.PairValue): String =
    val builder        = new StringBuilder("(")
    var current: Value = pair
    var first          = true

    while true do
      current match
        case Value.PairValue(car, cdr) =>
          if !first then builder.append(" ")
          builder.append(render(car))
          current = cdr
          first = false
        case Value.NilValue =>
          builder.append(")")
          return builder.result()
        case other =>
          builder.append(" . ")
          builder.append(render(other))
          builder.append(")")
          return builder.result()

    builder.result()

  private def escapeString(text: String): String =
    text.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  private def requireArgCount(name: String, args: List[Value], exact: Int): Unit =
    if args.length != exact then throw new EvalError(s"$name expected $exact argument(s), got ${args.length}")

  private def requireMinArgCount(name: String, args: List[Value], minimum: Int): Unit =
    if args.length < minimum then
      throw new EvalError(s"$name expected at least $minimum argument(s), got ${args.length}")

  private def numericArgs(name: String, args: List[Value]): List[BigInt] =
    args.map {
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"$name expected a number, got ${render(other)}")
    }

  private def numericComparator(name: String)(predicate: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        val numbers = numericArgs(name, args)
        requireMinArgCount(name, args, 2)
        Value.BooleanValue(numbers.zip(numbers.tail).forall(predicate.tupled))
    )

  private def baseEnv(): Env =
    val env = new Env(None)
    builtinBindings.foreach { case (name, value) =>
      env.define(name, value)
    }

    env

  private val builtinBindings: List[(String, Value)] = List(
    "+" -> Value.Builtin(
      "+",
      args => Value.IntegerValue(numericArgs("+", args).foldLeft(BigInt(0))(_ + _))
    ),
    "-" -> Value.Builtin(
      "-",
      args =>
        val numbers = numericArgs("-", args)
        requireMinArgCount("-", args, 1)
        val result =
          if numbers.length == 1 then -numbers.head
          else numbers.tail.foldLeft(numbers.head)(_ - _)
        Value.IntegerValue(result)
    ),
    "*" -> Value.Builtin(
      "*",
      args => Value.IntegerValue(numericArgs("*", args).foldLeft(BigInt(1))(_ * _))
    ),
    "/" -> Value.Builtin(
      "/",
      args =>
        val numbers = numericArgs("/", args)
        requireMinArgCount("/", args, 2)
        val result = numbers.tail.foldLeft(numbers.head) { (left, right) =>
          if right == 0 then throw new EvalError("division by zero")
          left / right
        }
        Value.IntegerValue(result)
    ),
    "<"  -> numericComparator("<")(_ < _),
    ">"  -> numericComparator(">")(_ > _),
    "="  -> numericComparator("=")(_ == _),
    "<=" -> numericComparator("<=")(_ <= _),
    "not" -> Value.Builtin(
      "not",
      args =>
        requireArgCount("not", args, 1)
        Value.BooleanValue(!isTruthy(args.head))
    )
  )
