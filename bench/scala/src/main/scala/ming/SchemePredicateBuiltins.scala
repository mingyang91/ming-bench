package ming

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeNumbers.*
import SchemeRuntime.*

private[ming] object SchemePredicateBuiltins:

  val bindings: List[(String, Value)] = List(
    "string?" -> predicateBuiltin("string?") {
      case Value.StringValue(_) => true
      case _                    => false
    },
    "char?" -> predicateBuiltin("char?") {
      case Value.CharValue(_) => true
      case _                  => false
    },
    "number?" -> predicateBuiltin("number?") { case value =>
      isNumber(value)
    },
    "boolean?" -> predicateBuiltin("boolean?") {
      case Value.BooleanValue(_) => true
      case _                     => false
    },
    "pair?" -> predicateBuiltin("pair?") {
      case Value.PairValue(_, _) => true
      case _                     => false
    },
    "list?" -> predicateBuiltin("list?")(isProperList),
    "symbol?" -> predicateBuiltin("symbol?") {
      case Value.SymbolValue(_) => true
      case _                    => false
    },
    "procedure?" -> predicateBuiltin("procedure?")(isProcedure)
  )
