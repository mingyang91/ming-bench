package ming

private[ming] object Level1VectorBuiltins:

  import Level1ValueSupport.isVector
  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "vector"        -> BuiltinValue("vector", vector),
    "make-vector"   -> BuiltinValue("make-vector", makeVector),
    "vector-ref"    -> BuiltinValue("vector-ref", vectorRef),
    "vector-set!"   -> BuiltinValue("vector-set!", vectorSet),
    "vector-length" -> BuiltinValue("vector-length", vectorLength),
    "vector->list"  -> BuiltinValue("vector->list", vectorToList),
    "list->vector"  -> BuiltinValue("list->vector", listToVector),
    "vector?"       -> BuiltinValue("vector?", vectorPredicate)
  )

  private def vector(arguments: List[Value], position: Position): Value =
    new VectorValue(arguments)

  private def makeVector(arguments: List[Value], position: Position): Value =
    val (length, fillValue) =
      arguments match
        case lengthValue :: Nil =>
          (expectIndex(lengthValue, "make-vector", position), VoidValue)
        case lengthValue :: initialValue :: Nil =>
          (expectIndex(lengthValue, "make-vector", position), initialValue)
        case _ =>
          SchemeFailure.raise(
            s"make-vector expected 1 or 2 argument(s), got ${arguments.length}",
            position
          )

    new VectorValue(List.fill(length)(fillValue))

  private def vectorRef(arguments: List[Value], position: Position): Value =
    val (vectorValue, indexValue) = expectTwoArguments(arguments, "vector-ref", position)
    val vector                    = expectVector(vectorValue, "vector-ref", position)
    vector.element(validIndex(vector, indexValue, "vector-ref", position))

  private def vectorSet(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 3, "vector-set!", position) match
      case vectorValue :: indexValue :: elementValue :: Nil =>
        val vector = expectVector(vectorValue, "vector-set!", position)
        vector.update(validIndex(vector, indexValue, "vector-set!", position), elementValue)
        VoidValue
      case _ =>
        throw new IllegalStateException("validated vector-set! argument list")

  private def vectorLength(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "vector-length", position)
    IntValue(expectVector(value, "vector-length", position).length)

  private def vectorToList(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "vector->list", position)
    buildList(expectVector(value, "vector->list", position).toList)

  private def listToVector(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "list->vector", position)
    new VectorValue(expectProperList(value, "list->vector", position))

  private def vectorPredicate(arguments: List[Value], position: Position): Value =
    BoolValue(isVector(expectSingleArgument(arguments, "vector?", position)))

  private def validIndex(
    vector: VectorValue,
    indexValue: Value,
    name: String,
    position: Position
  ): Int =
    val index = expectIndex(indexValue, name, position)
    if index >= vector.length then SchemeFailure.raise(s"$name index out of bounds", position)

    index
