package ming

import scala.collection.mutable

private[ming] object Level1ListSearchSupport:

  import RuntimeSupport.typeName

  def membershipValue(
    key: Value,
    value: Value,
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val visited = mutable.HashSet.empty[PairValue]
    var current = value

    while true do
      current match
        case EmptyListValue =>
          return BoolValue(false)
        case pair: PairValue =>
          if visited.contains(pair) then
            SchemeFailure.raise(s"$name expected a proper list, got circular list", position)

          visited += pair
          if matches(key, pair.car) then return pair
          current = pair.cdr
        case other =>
          SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

    throw new IllegalStateException("unreachable membership traversal")

  def associationValue(
    key: Value,
    value: Value,
    name: String,
    position: Position
  )(matches: (Value, Value) => Boolean): Value =
    val visited = mutable.HashSet.empty[PairValue]
    var current = value

    while true do
      current match
        case EmptyListValue =>
          return BoolValue(false)
        case pair: PairValue =>
          if visited.contains(pair) then
            SchemeFailure.raise(s"$name expected a proper list, got circular list", position)

          visited += pair
          pair.car match
            case entry: PairValue =>
              if matches(key, entry.car) then return entry
              current = pair.cdr
            case other =>
              SchemeFailure.raise(
                s"$name expected pairs in its association list, got ${typeName(other)}",
                position
              )
        case other =>
          SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

    throw new IllegalStateException("unreachable association traversal")
