package ming

private[ming] object PredicateBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      predicateBuiltin("string?") {
        case Value.StringLit(_)     => true
        case _: Value.MutableString => true
        case _                      => false
      },
      predicateBuiltin("number?") {
        case Value.Number(_) => true
        case _               => false
      },
      predicateBuiltin("integer?") {
        case Value.Number(number) => number.isInteger
        case _                    => false
      },
      predicateBuiltin("rational?") {
        case Value.Number(_) => true
        case _               => false
      },
      predicateBuiltin("exact?") {
        case Value.Number(number) => number.isExact
        case _                    => false
      },
      predicateBuiltin("inexact?") {
        case Value.Number(number) => number.isInexact
        case _                    => false
      },
      predicateBuiltin("boolean?") {
        case Value.Bool(_) => true
        case _             => false
      },
      predicateBuiltin("pair?") {
        case Value.Pair(_, _) => true
        case _                => false
      },
      predicateBuiltin("symbol?") {
        case Value.Symbol(_) => true
        case _               => false
      },
      predicateBuiltin("char?") {
        case Value.Character(_) => true
        case _                  => false
      },
      predicateBuiltin("list?") { case value =>
        isProperList(value)
      }
    )

  private def predicateBuiltin(
    name: String
  )(predicate: Value => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(singleArg(name, args, pos)))
    )
