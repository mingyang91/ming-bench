package ming

import scala.annotation.tailrec

private[ming] object Level1Interpreter:

  def evalProgram(input: String): String =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then SchemeFailure.raise("expected expression", Position(1, 1))

    val env    = Environment.root(builtins)
    val result = evalSequence(expressions, env)

    result.render

  private val builtins: Map[String, Value] = Map(
    "+"   -> BuiltinValue("+", add),
    "-"   -> BuiltinValue("-", subtract),
    "*"   -> BuiltinValue("*", multiply),
    "/"   -> BuiltinValue("/", divide),
    "<"   -> BuiltinValue("<", compareNumbers("<")(_ < _)),
    ">"   -> BuiltinValue(">", compareNumbers(">")(_ > _)),
    "="   -> BuiltinValue("=", compareNumbers("=")(_ == _)),
    "<="  -> BuiltinValue("<=", compareNumbers("<=")(_ <= _)),
    "not" -> BuiltinValue("not", logicalNot)
  )

  private def evalSequence(expressions: List[Expr], env: Environment): Value =
    expressions.foldLeft[Value](VoidValue): (_, expression) =>
      eval(expression, env)

  private def eval(expression: Expr, env: Environment): Value =
    expression match
      case IntExpr(value, _)    => IntValue(value)
      case BoolExpr(value, _)   => BoolValue(value)
      case StringExpr(value, _) => StringValue(value)
      case SymbolExpr(name, position) =>
        env.lookup(name, position)
      case ListExpr(items, position) =>
        evalList(items, position, env)

  private def evalList(items: List[Expr], position: Position, env: Environment): Value =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        evalAnd(rest, env)
      case SymbolExpr("or", _) :: rest =>
        evalOr(rest, env)
      case SymbolExpr("define", _) :: rest =>
        evalDefine(rest, position, env)
      case SymbolExpr("if", _) :: rest =>
        evalIf(rest, position, env)
      case SymbolExpr("quote", _) :: rest =>
        evalQuote(rest, position)
      case SymbolExpr("lambda", _) :: rest =>
        evalLambda(rest, position, env)
      case operator :: arguments =>
        val function           = eval(operator, env)
        val evaluatedArguments = arguments.map(argument => eval(argument, env))
        apply(function, evaluatedArguments, position)

  @tailrec
  private def evalAnd(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(true)
      case expression :: Nil =>
        eval(expression, env)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then evalAnd(rest, env)
        else result

  @tailrec
  private def evalOr(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(false)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then result
        else evalOr(rest, env)

  private def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, _) :: valueExpression :: Nil =>
        env.reserve(name)
        val value = eval(valueExpression, env)
        env.define(name, value)
        VoidValue
      case ListExpr(SymbolExpr(name, _) :: parameters, _) :: body if body.nonEmpty =>
        env.reserve(name)
        val value = buildClosure(parameters, body, env, Some(name), position)
        env.define(name, value)
        VoidValue
      case _ =>
        SchemeFailure.raise(
          "define expected (define name expr) or (define (name args) body ...)",
          position
        )

  private def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        if isTruthy(eval(condition, env)) then eval(consequent, env)
        else eval(alternate, env)
      case _ =>
        SchemeFailure.raise("if expected 3 argument(s)", position)

  private def evalQuote(arguments: List[Expr], position: Position): Value =
    arguments match
      case expression :: Nil =>
        quote(expression)
      case _ =>
        SchemeFailure.raise("quote expected 1 argument(s)", position)

  private def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case ListExpr(parameters, _) :: body if body.nonEmpty =>
        buildClosure(parameters, body, env, None, position)
      case _ =>
        SchemeFailure.raise("lambda expected a parameter list and body", position)

  private def buildClosure(
    parameterExpressions: List[Expr],
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val parameters = parameterExpressions.map:
      case SymbolExpr(parameterName, _) => parameterName
      case _ =>
        SchemeFailure.raise("lambda parameters must be symbols", position)

    ClosureValue(parameters, body, env, name)

  private def quote(expression: Expr): Value =
    expression match
      case IntExpr(value, _)    => IntValue(value)
      case BoolExpr(value, _)   => BoolValue(value)
      case StringExpr(value, _) => StringValue(value)
      case SymbolExpr(name, _)  => SymbolValue(name)
      case ListExpr(items, _) =>
        items.foldRight[Value](EmptyListValue): (item, rest) =>
          PairValue(quote(item), rest)

  private def apply(function: Value, arguments: List[Value], position: Position): Value =
    function match
      case BuiltinValue(_, implementation) =>
        implementation(arguments, position)
      case ClosureValue(parameters, body, closureEnv, _) =>
        if arguments.length != parameters.length then
          SchemeFailure.raise(
            s"procedure expected ${parameters.length} argument(s), got ${arguments.length}",
            position
          )

        val callEnv = Environment.child(closureEnv, parameters.zip(arguments))
        evalSequence(body, callEnv)
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def add(arguments: List[Value], position: Position): Value =
    IntValue(arguments.map(expectNumber(_, "+", position)).foldLeft(BigInt(0))(_ + _))

  private def subtract(arguments: List[Value], position: Position): Value =
    val numbers = expectAtLeast(arguments, 1, "-", position).map(expectNumber(_, "-", position))
    val result =
      numbers match
        case number :: Nil  => -number
        case number :: rest => rest.foldLeft(number)(_ - _)
        case Nil            => unreachable("validated non-empty argument list")
    IntValue(result)

  private def multiply(arguments: List[Value], position: Position): Value =
    IntValue(arguments.map(expectNumber(_, "*", position)).foldLeft(BigInt(1))(_ * _))

  private def divide(arguments: List[Value], position: Position): Value =
    val numbers = expectAtLeast(arguments, 1, "/", position).map(expectNumber(_, "/", position))
    val result =
      numbers match
        case denominator :: Nil =>
          divideExactly(BigInt(1), denominator, position)
        case numerator :: rest =>
          rest.foldLeft(numerator)((accumulator, denominator) => divideExactly(accumulator, denominator, position))
        case Nil =>
          unreachable("validated non-empty argument list")
    IntValue(result)

  private def divideExactly(
    numerator: BigInt,
    denominator: BigInt,
    position: Position
  ): BigInt =
    if denominator == 0 then SchemeFailure.raise("division by zero", position)

    if numerator % denominator != 0 then SchemeFailure.raise("division produced a non-integer result", position)

    numerator / denominator

  private def compareNumbers(
    name: String
  )(predicate: (BigInt, BigInt) => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val numbers = expectAtLeast(arguments, 2, name, position).map(expectNumber(_, name, position))
      BoolValue(numbers.zip(numbers.tail).forall((left, right) => predicate(left, right)))

  private def logicalNot(arguments: List[Value], position: Position): Value =
    val value =
      expectExact(arguments, 1, "not", position) match
        case argument :: Nil => argument
        case _               => unreachable("validated single argument list")

    BoolValue(!isTruthy(value))

  private def expectExact(
    arguments: List[Value],
    expected: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length != expected then
      SchemeFailure.raise(s"$name expected $expected argument(s), got ${arguments.length}", position)

    arguments

  private def expectAtLeast(
    arguments: List[Value],
    minimum: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length < minimum then
      SchemeFailure.raise(s"$name expected at least $minimum argument(s), got ${arguments.length}", position)

    arguments

  private def expectNumber(value: Value, name: String, position: Position): BigInt =
    value match
      case IntValue(number) => number
      case other =>
        SchemeFailure.raise(s"$name expected a number, got ${typeName(other)}", position)

  private def isTruthy(value: Value): Boolean =
    value match
      case BoolValue(false) => false
      case _                => true

  private def typeName(value: Value): String =
    value match
      case IntValue(_)        => "number"
      case BoolValue(_)       => "boolean"
      case StringValue(_)     => "string"
      case SymbolValue(_)     => "symbol"
      case EmptyListValue     => "list"
      case PairValue(_, _)    => "pair"
      case BuiltinValue(_, _) => "procedure"
      case ClosureValue(_, _, _, _) =>
        "procedure"
      case VoidValue =>
        "void"
      case UninitializedValue =>
        "uninitialized"

  private def unreachable(message: String): Nothing =
    throw new IllegalStateException(message)
