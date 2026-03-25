package ming

private[ming] object VectorBuiltins extends BuiltinSupport:

  val entries: Map[String, Value] = Map(
    "vector"        -> Value.Builtin("vector", vector),
    "make-vector"   -> Value.Builtin("make-vector", makeVector),
    "vector-ref"    -> Value.Builtin("vector-ref", vectorRef),
    "vector-set!"   -> Value.Builtin("vector-set!", vectorSet),
    "vector-length" -> Value.Builtin("vector-length", vectorLength),
    "vector?"       -> Value.Builtin("vector?", vectorPredicate),
    "vector->list"  -> Value.Builtin("vector->list", vectorToList),
    "list->vector"  -> Value.Builtin("list->vector", listToVector)
  )

  private def vector(args: List[Value], pos: SourcePos): Value =
    Value.VectorVal(args.toArray)

  private def makeVector(args: List[Value], pos: SourcePos): Value =
    args match
      case sizeValue :: fillValue :: Nil =>
        Value.VectorVal(Array.fill(expectIndex(sizeValue, pos, "make-vector"))(fillValue))

      case sizeValue :: Nil =>
        Value.VectorVal(Array.fill(expectIndex(sizeValue, pos, "make-vector"))(Value.VoidVal))

      case _ =>
        throw EvalError.at(pos, "make-vector expects 1 or 2 arguments")

  private def vectorRef(args: List[Value], pos: SourcePos): Value =
    val (vectorValue, indexValue) = expectTwoArgs(args, pos, "vector-ref")
    val elements                  = expectVector(vectorValue, pos, "vector-ref")
    val index                     = expectIndex(indexValue, pos, "vector-ref")
    if index >= elements.length then throw EvalError.at(pos, "vector-ref index is out of bounds")

    elements(index)

  private def vectorSet(args: List[Value], pos: SourcePos): Value =
    args match
      case vectorValue :: indexValue :: newValue :: Nil =>
        val elements = expectVector(vectorValue, pos, "vector-set!")
        val index    = expectIndex(indexValue, pos, "vector-set!")
        if index >= elements.length then throw EvalError.at(pos, "vector-set! index is out of bounds")

        elements(index) = newValue
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "vector-set! expects exactly 3 arguments")

  private def vectorLength(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(expectVector(expectSingleArg(args, pos, "vector-length"), pos, "vector-length").length)

  private def vectorPredicate(args: List[Value], pos: SourcePos): Value =
    expectSingleArg(args, pos, "vector?") match
      case Value.VectorVal(_) =>
        Value.BoolVal(true)

      case _ =>
        Value.BoolVal(false)

  private def vectorToList(args: List[Value], pos: SourcePos): Value =
    Value.list(expectVector(expectSingleArg(args, pos, "vector->list"), pos, "vector->list").toList)

  private def listToVector(args: List[Value], pos: SourcePos): Value =
    Value.VectorVal(asProperList(expectSingleArg(args, pos, "list->vector"), pos, "list->vector").toArray)

  private def expectVector(arg: Value, pos: SourcePos, name: String): Array[Value] =
    arg match
      case Value.VectorVal(elements) =>
        elements

      case other =>
        throw EvalError.at(pos, s"$name expected a vector, got ${other.typeName}")
