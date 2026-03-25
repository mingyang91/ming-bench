package ming

private[ming] object VectorBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      vectorBuiltin,
      makeVectorBuiltin,
      vectorRefBuiltin,
      vectorSetBuiltin,
      vectorLengthBuiltin,
      vectorPredicateBuiltin,
      vectorToListBuiltin,
      listToVectorBuiltin
    )

  private val vectorBuiltin: Value.Builtin =
    Value.Builtin(
      "vector",
      (args, _) => Value.Vector(args)
    )

  private val makeVectorBuiltin: Value.Builtin =
    Value.Builtin(
      "make-vector",
      (args, pos) =>
        val (length, fill) =
          args match
            case lengthValue :: Nil =>
              (asIndex(lengthValue, "make-vector", pos), Value.Void)
            case lengthValue :: fillValue :: Nil =>
              (asIndex(lengthValue, "make-vector", pos), fillValue)
            case _ =>
              fail(pos, s"make-vector expected 1 or 2 arguments, got ${args.length}")

        Value.Vector(List.fill(length)(fill))
    )

  private val vectorRefBuiltin: Value.Builtin =
    Value.Builtin(
      "vector-ref",
      (args, pos) =>
        val (vectorValue, indexValue) = twoArgs("vector-ref", args, pos)
        val vector                    = asVector(vectorValue, "vector-ref", pos)
        val index                     = asIndex(indexValue, "vector-ref", pos)
        requireIndexInRange(index, vector.length, "vector-ref", pos)
        vector.elementAt(index)
    )

  private val vectorSetBuiltin: Value.Builtin =
    Value.Builtin(
      "vector-set!",
      (args, pos) =>
        val (vectorValue, indexValue, newValue) = threeArgs("vector-set!", args, pos)
        val vector                              = asVector(vectorValue, "vector-set!", pos)
        val index                               = asIndex(indexValue, "vector-set!", pos)
        requireIndexInRange(index, vector.length, "vector-set!", pos)
        vector.set(index, newValue)
        Value.Void
    )

  private val vectorLengthBuiltin: Value.Builtin =
    Value.Builtin(
      "vector-length",
      (args, pos) =>
        val vector = asVector(singleArg("vector-length", args, pos), "vector-length", pos)
        Value.Number(SchemeNumber.exact(BigInt(vector.length)))
    )

  private val vectorPredicateBuiltin: Value.Builtin =
    Value.Builtin(
      "vector?",
      (args, pos) =>
        val value = singleArg("vector?", args, pos)
        Value.Bool(value.isInstanceOf[Value.Vector])
    )

  private val vectorToListBuiltin: Value.Builtin =
    Value.Builtin(
      "vector->list",
      (args, pos) =>
        val vector = asVector(singleArg("vector->list", args, pos), "vector->list", pos)
        Value.list(vector.toList)
    )

  private val listToVectorBuiltin: Value.Builtin =
    Value.Builtin(
      "list->vector",
      (args, pos) => Value.Vector(asList(singleArg("list->vector", args, pos), "list->vector", pos))
    )
