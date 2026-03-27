package ming

import BuiltinSupport.*

private[ming] object Builtins:

  private val builtinNames = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not",
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
    "procedure?",
    "eq?",
    "eqv?",
    "equal?",
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "numerator",
    "denominator",
    "min",
    "max",
    "expt",
    "exact->inexact",
    "inexact->exact",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "map",
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?",
    "apply"
  ) ++ ListBuiltins.names ++ OutputBuiltins.names ++ StringBuiltins.names ++ VectorBuiltins.names

  def resolve(name: String): Option[Value] =
    if builtinNames.contains(name) then Some(Value.BuiltinProc(name))
    else None

  def invoke(name: String, args: List[Value], pos: SourcePos, context: EvalContext): Value =
    name match
      case "+" | "-" | "*" | "/" =>
        invokeArithmetic(name, args, pos)
      case "<" | ">" | "=" | "<=" | ">=" =>
        invokeComparison(name, args, pos)
      case "not" =>
        Value.BoolVal(!ValueSemantics.isTruthy(requireSingleArg(name, args, pos)))
      case "eq?" | "eqv?" | "equal?" =>
        invokeEqualityBuiltin(name, args, pos)
      case "abs" | "modulo" | "remainder" | "quotient" | "numerator" | "denominator" | "min" | "max" | "expt" |
          "exact->inexact" | "inexact->exact" =>
        invokeNumericUtility(name, args, pos)
      case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
        invokeNumericPredicate(name, args, pos)
      case builtin if ListBuiltins.handlesCore(builtin) =>
        ListBuiltins.invokeCore(builtin, args, pos)
      case builtin if ListBuiltins.handlesUtility(builtin) =>
        ListBuiltins.invokeUtility(builtin, args, pos)
      case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
        invokeCharBuiltin(name, args, pos)
      case "null?" | "pair?" | "number?" | "exact?" | "inexact?" | "integer?" | "rational?" | "string?" | "boolean?" |
          "symbol?" | "char?" | "procedure?" =>
        invokePredicateBuiltin(name, args, pos)
      case builtin if OutputBuiltins.handles(builtin) =>
        OutputBuiltins.invoke(builtin, args, pos, context)
      case builtin if StringBuiltins.handles(builtin) =>
        StringBuiltins.invoke(builtin, args, pos)
      case builtin if VectorBuiltins.handles(builtin) =>
        VectorBuiltins.invoke(builtin, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeArithmetic(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "+" =>
        SchemeNumber.add(evalNumbers(name, args, pos)).toValue
      case "-" =>
        subtract(name, args, pos)
      case "*" =>
        SchemeNumber.multiply(evalNumbers(name, args, pos)).toValue
      case "/" =>
        divide(name, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def subtract(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)
    SchemeNumber.subtract(numbers).toValue

  private def divide(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
    SchemeNumber.divide(numbers, pos).toValue

  private def invokeComparison(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "<" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) < 0)
      case ">" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) > 0)
      case "=" =>
        compareNumbers(name, args, pos)(SchemeNumber.areEqual)
      case "<=" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) <= 0)
      case ">=" =>
        compareNumbers(name, args, pos)((left, right) => SchemeNumber.compare(left, right) >= 0)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeEqualityBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    name match
      case "eq?" =>
        Value.BoolVal(ValueSemantics.isEq(values.head, values(1)))
      case "eqv?" =>
        Value.BoolVal(ValueSemantics.isEqv(values.head, values(1)))
      case "equal?" =>
        Value.BoolVal(ValueSemantics.isEqual(values.head, values(1)))
      case _ =>
        unknownProcedure(name, pos)

  private def invokeNumericUtility(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "abs" =>
        SchemeNumber.abs(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case "modulo" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        Value.IntVal(modulo(dividend, divisor, pos))
      case "remainder" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend % divisor)
      case "quotient" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend / divisor)
      case "numerator" =>
        Value.IntVal(SchemeNumber.numerator(requireExactNumber(name, args, pos)))
      case "denominator" =>
        Value.IntVal(SchemeNumber.denominator(requireExactNumber(name, args, pos)))
      case "min" =>
        SchemeNumber.min(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)).toValue
      case "max" =>
        SchemeNumber.max(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)).toValue
      case "expt" =>
        val (base, exponent) = requireBinaryExactIntegers(name, args, pos)
        if exponent < 0 then throw EvalError.at(pos, s"$name expected a non-negative exponent")
        Value.IntVal(expt(base, exponent))
      case "exact->inexact" =>
        SchemeNumber.exactToInexact(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case "inexact->exact" =>
        SchemeNumber.inexactToExact(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case _ =>
        unknownProcedure(name, pos)

  private def invokeNumericPredicate(name: String, args: List[Value], pos: SourcePos): Value =
    val number = requireNumber(name, requireSingleArg(name, args, pos), pos)
    name match
      case "zero?" =>
        Value.BoolVal(SchemeNumber.isZero(number))
      case "positive?" =>
        Value.BoolVal(SchemeNumber.compare(number, SchemeNumber.ExactInt(0L)) > 0)
      case "negative?" =>
        Value.BoolVal(SchemeNumber.compare(number, SchemeNumber.ExactInt(0L)) < 0)
      case "odd?" =>
        Value.BoolVal(requireExactInteger(name, requireSingleArg(name, args, pos), pos) % 2L != 0L)
      case "even?" =>
        Value.BoolVal(requireExactInteger(name, requireSingleArg(name, args, pos), pos) % 2L == 0L)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeCharBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
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
      case _ =>
        unknownProcedure(name, pos)

  private def invokePredicateBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    name match
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
        unaryPredicate(name, args, pos) { case value =>
          SchemeNumber.fromValue(value).isDefined
        }
      case "exact?" =>
        unaryPredicate(name, args, pos) { case value =>
          SchemeNumber.fromValue(value).exists(_.isExact)
        }
      case "inexact?" =>
        unaryPredicate(name, args, pos) { case value =>
          SchemeNumber.fromValue(value).exists(!_.isExact)
        }
      case "integer?" =>
        unaryPredicate(name, args, pos) { case value =>
          SchemeNumber.fromValue(value).exists(_.isInteger)
        }
      case "rational?" =>
        unaryPredicate(name, args, pos) { case value =>
          SchemeNumber.fromValue(value).exists(_.isRational)
        }
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

  private def requireExactNumber(name: String, args: List[Value], pos: SourcePos): SchemeNumber =
    requireNumber(name, requireSingleArg(name, args, pos), pos) match
      case exact if exact.isExact => exact
      case _                      => throw EvalError.at(pos, s"$name expected an exact number")

  private def requireBinaryExactIntegers(name: String, args: List[Value], pos: SourcePos): (Long, Long) =
    val values = requireArgCount(name, args, expected = 2, pos)
    (
      requireExactInteger(name, values.head, pos),
      requireExactInteger(name, values(1), pos)
    )

  private def modulo(dividend: Long, divisor: Long, pos: SourcePos): Long =
    if divisor == 0 then throw EvalError.at(pos, "division by zero")
    val remainder = dividend % divisor
    if remainder == 0 || java.lang.Long.signum(remainder) == java.lang.Long.signum(divisor) then remainder
    else remainder + divisor

  private def expt(base: Long, exponent: Long): Long =
    @annotation.tailrec
    def loop(factor: Long, power: Long, acc: Long): Long =
      if power == 0 then acc
      else if (power & 1) == 1 then loop(factor * factor, power >>> 1, acc * factor)
      else loop(factor * factor, power >>> 1, acc)

    loop(base, exponent, acc = 1L)
