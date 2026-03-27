package ming

import BuiltinSupport.*

private[ming] object VectorBuiltins:

  val names: Set[String] = Set(
    "vector",
    "make-vector",
    "vector-ref",
    "vector-set!",
    "vector-length",
    "vector?",
    "vector->list",
    "list->vector"
  )

  def handles(name: String): Boolean =
    names.contains(name)

  def invoke(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "vector" =>
        Value.VectorVal(new VectorInstance(args.toArray))
      case "make-vector" =>
        makeVector(args, pos)
      case "vector-ref" =>
        vectorRef(args, pos)
      case "vector-set!" =>
        vectorSet(args, pos)
      case "vector-length" =>
        vectorLength(args, pos)
      case "vector?" =>
        Value.BoolVal(
          requireSingleArg(name, args, pos) match
            case Value.VectorVal(_) => true
            case _                  => false
        )
      case "vector->list" =>
        val vector = requireVector(name, requireSingleArg(name, args, pos), pos)
        ValueSemantics.listFrom(vector.elements)
      case "list->vector" =>
        val elements = ValueSemantics.toProperList(name, requireSingleArg(name, args, pos), pos)
        Value.VectorVal(new VectorInstance(elements.toArray))
      case _ =>
        unknownProcedure(name, pos)

  private def makeVector(args: List[Value], pos: SourcePos): Value =
    if args.lengthCompare(1) < 0 || args.lengthCompare(2) > 0 then
      throw EvalError.at(pos, s"make-vector expects 1 or 2 argument(s), got ${args.length}")

    val length = requireVectorLength("make-vector", args.head, pos)
    val fill   = args.lift(1).getOrElse(Value.Void)
    Value.VectorVal(new VectorInstance(Array.fill(length)(fill)))

  private def vectorRef(args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount("vector-ref", args, expected = 2, pos)
    val vector = requireVector("vector-ref", values.head, pos)
    vector.elementAt(requireVectorIndex("vector-ref", vector, values(1), pos))

  private def vectorSet(args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount("vector-set!", args, expected = 3, pos)
    val vector = requireVector("vector-set!", values.head, pos)
    val index  = requireVectorIndex("vector-set!", vector, values(1), pos)
    vector.setElementAt(index, values(2))
    Value.Void

  private def vectorLength(args: List[Value], pos: SourcePos): Value =
    val vector = requireVector("vector-length", requireSingleArg("vector-length", args, pos), pos)
    Value.IntVal(vector.length.toLong)

  private def requireVectorLength(name: String, value: Value, pos: SourcePos): Int =
    val length = requireIndex(name, value, pos)
    if length < 0 then throw EvalError.at(pos, s"$name expected a non-negative length")
    length

  private def requireVectorIndex(
    name: String,
    vector: VectorInstance,
    value: Value,
    pos: SourcePos
  ): Int =
    val index = requireIndex(name, value, pos)
    if index < 0 || index >= vector.length then throw EvalError.at(pos, s"$name index out of range")
    index
