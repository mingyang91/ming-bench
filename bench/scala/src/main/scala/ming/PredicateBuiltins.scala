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
      predicateBuiltin("boolean?") {
        case Value.Bool(_) => true
        case _             => false
      },
      predicateBuiltin("pair?") {
        case Value.ListValue(_ :: _) => true
        case _                       => false
      },
      predicateBuiltin("symbol?") {
        case Value.Symbol(_) => true
        case _               => false
      },
      predicateBuiltin("char?") {
        case Value.Character(_) => true
        case _                  => false
      }
    )

  private def predicateBuiltin(
    name: String
  )(predicate: Value => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(singleArg(name, args, pos)))
    )
