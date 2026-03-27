package ming

private[ming] object Level1PredicateBuiltins:

  import RuntimeSupport.*
  import Level1ValueSupport.*

  val values: Map[String, Value] = Map(
    "not"      -> BuiltinValue("not", logicalNot),
    "eq?"      -> BuiltinValue("eq?", eqValue),
    "equal?"   -> BuiltinValue("equal?", equalValue),
    "null?"    -> BuiltinValue("null?", unaryPredicate("null?")(_ == EmptyListValue)),
    "list?"    -> BuiltinValue("list?", unaryPredicate("list?")(isProperList)),
    "string?"  -> BuiltinValue("string?", unaryPredicate("string?")(isString)),
    "number?"  -> BuiltinValue("number?", unaryPredicate("number?")(isNumber)),
    "boolean?" -> BuiltinValue("boolean?", unaryPredicate("boolean?")(isBoolean)),
    "pair?"    -> BuiltinValue("pair?", unaryPredicate("pair?")(isPair)),
    "symbol?"  -> BuiltinValue("symbol?", unaryPredicate("symbol?")(isSymbol)),
    "char?"    -> BuiltinValue("char?", unaryPredicate("char?")(isChar))
  )

  private def logicalNot(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "not", position)
    BoolValue(!isTruthy(value))

  private def eqValue(arguments: List[Value], position: Position): Value =
    val (left, right) = expectTwoArguments(arguments, "eq?", position)
    BoolValue(eqValues(left, right))

  private def equalValue(arguments: List[Value], position: Position): Value =
    val (left, right) = expectTwoArguments(arguments, "equal?", position)
    BoolValue(equalValues(left, right))

  private def unaryPredicate(
    name: String
  )(predicate: Value => Boolean): (List[Value], Position) => Value =
    (arguments, position) => BoolValue(predicate(expectSingleArgument(arguments, name, position)))
