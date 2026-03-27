package ming

private[ming] object MultiValueSupport:

  def pack(values: List[Value]): Value =
    values match
      case value :: Nil => value
      case _            => Value.MultiValues(values)

  def unpack(value: Value): List[Value] =
    value match
      case Value.MultiValues(values) => values
      case other                     => List(other)

  def requireSingle(value: Value, pos: SourcePos, context: String): Value =
    value match
      case Value.MultiValues(values) if values.lengthCompare(1) != 0 =>
        throw EvalError.at(pos, s"$context expected 1 value, got ${values.length}")
      case Value.MultiValues(value :: Nil) =>
        value
      case other =>
        other
