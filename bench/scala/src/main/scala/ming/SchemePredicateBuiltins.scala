package ming

import BuiltinSupport.*

private[ming] object PredicateBuiltins:

  private val typePredicateNames = Set(
    "null?",
    "pair?",
    "number?",
    "exact?",
    "inexact?",
    "integer?",
    "rational?",
    "string?",
    "boolean?",
    "symbol?",
    "char?",
    "procedure?"
  )

  private val charBuiltinNames = Set(
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?"
  )

  val names: Set[String] = typePredicateNames ++ charBuiltinNames

  def handles(name: String): Boolean =
    names.contains(name)

  def invoke(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "char-alphabetic?" =>
        Value.BoolVal(Character.isLetter(requireChar(name, requireSingleArg(name, args, pos), pos)))
      case "char-numeric?" =>
        Value.BoolVal(Character.isDigit(requireChar(name, requireSingleArg(name, args, pos), pos)))
      case "char-upcase" =>
        Value.CharVal(Character.toUpperCase(requireChar(name, requireSingleArg(name, args, pos), pos)))
      case "char-downcase" =>
        Value.CharVal(Character.toLowerCase(requireChar(name, requireSingleArg(name, args, pos), pos)))
      case "char=?" =>
        compareChars(name, args, pos)(_ == _)
      case "char<?" =>
        compareChars(name, args, pos)(_ < _)
      case "null?" =>
        unaryPredicate(name, args, pos) {
          case Value.EmptyList => true
          case _               => false
        }
      case "pair?" =>
        unaryPredicate(name, args, pos) {
          case Value.PairVal(_, _) => true
          case _                   => false
        }
      case "number?" =>
        unaryPredicate(name, args, pos)(value => SchemeNumber.fromValue(value).isDefined)
      case "exact?" =>
        unaryPredicate(name, args, pos)(value => SchemeNumber.fromValue(value).exists(_.isExact))
      case "inexact?" =>
        unaryPredicate(name, args, pos)(value => SchemeNumber.fromValue(value).exists(!_.isExact))
      case "integer?" =>
        unaryPredicate(name, args, pos)(value => SchemeNumber.fromValue(value).exists(_.isInteger))
      case "rational?" =>
        unaryPredicate(name, args, pos)(value => SchemeNumber.fromValue(value).exists(_.isRational))
      case "string?" =>
        unaryPredicate(name, args, pos) {
          case Value.StringVal(_) => true
          case _                  => false
        }
      case "boolean?" =>
        unaryPredicate(name, args, pos) {
          case Value.BoolVal(_) => true
          case _                => false
        }
      case "symbol?" =>
        unaryPredicate(name, args, pos) {
          case Value.SymbolVal(_) => true
          case _                  => false
        }
      case "char?" =>
        unaryPredicate(name, args, pos) {
          case Value.CharVal(_) => true
          case _                => false
        }
      case "procedure?" =>
        unaryPredicate(name, args, pos)(ValueSemantics.isProcedure)
      case _ =>
        unknownProcedure(name, pos)
