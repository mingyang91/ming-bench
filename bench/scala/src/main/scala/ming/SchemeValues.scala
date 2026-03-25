package ming

private[ming] object SchemeValues:
  import SchemeInterpreter.Value

  def pack(values: List[Value]): Value =
    values match
      case value :: Nil => value
      case _            => Value.MultipleValues(values)

  def unpack(value: Value): List[Value] =
    value match
      case Value.MultipleValues(values) => values
      case other                        => List(other)
