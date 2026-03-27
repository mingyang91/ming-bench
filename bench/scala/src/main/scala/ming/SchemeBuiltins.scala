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
    "not",
    "null?",
    "pair?",
    "number?",
    "string?",
    "boolean?",
    "symbol?",
    "char?",
    "eq?",
    "equal?",
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "min",
    "max",
    "expt",
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
  ) ++ ListBuiltins.names ++ OutputBuiltins.names ++ StringBuiltins.names

  def resolve(name: String): Option[Value] =
    if builtinNames.contains(name) then Some(Value.BuiltinProc(name))
    else None

  def invoke(name: String, args: List[Value], pos: SourcePos, context: EvalContext): Value =
    name match
      case "+" | "-" | "*" | "/" =>
        invokeArithmetic(name, args, pos)
      case "<" | ">" | "=" | "<=" =>
        invokeComparison(name, args, pos)
      case "not" =>
        Value.BoolVal(!ValueSemantics.isTruthy(requireSingleArg(name, args, pos)))
      case "eq?" | "equal?" =>
        invokeEqualityBuiltin(name, args, pos)
      case "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" =>
        invokeNumericUtility(name, args, pos)
      case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
        invokeNumericPredicate(name, args, pos)
      case builtin if ListBuiltins.handlesCore(builtin) =>
        ListBuiltins.invokeCore(builtin, args, pos)
      case builtin if ListBuiltins.handlesUtility(builtin) =>
        ListBuiltins.invokeUtility(builtin, args, pos)
      case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
        invokeCharBuiltin(name, args, pos)
      case "null?" | "pair?" | "number?" | "string?" | "boolean?" | "symbol?" | "char?" =>
        invokePredicateBuiltin(name, args, pos)
      case builtin if OutputBuiltins.handles(builtin) =>
        OutputBuiltins.invoke(builtin, args, pos, context)
      case builtin if StringBuiltins.handles(builtin) =>
        StringBuiltins.invoke(builtin, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeArithmetic(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "+" =>
        Value.IntVal(evalNumbers(name, args, pos).sum)
      case "-" =>
        subtract(name, args, pos)
      case "*" =>
        Value.IntVal(evalNumbers(name, args, pos).product)
      case "/" =>
        divide(name, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def subtract(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)
    numbers match
      case value :: Nil =>
        Value.IntVal(-value)
      case value :: rest =>
        Value.IntVal(rest.foldLeft(value)(_ - _))
      case Nil =>
        throw EvalError.at(pos, s"$name expects at least 1 argument")

  private def divide(name: String, args: List[Value], pos: SourcePos): Value =
    val numbers = requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos)
    val result = numbers.tail.foldLeft(numbers.head) { (acc, divisor) =>
      if divisor == 0 then throw EvalError.at(pos, "division by zero")
      acc / divisor
    }
    Value.IntVal(result)

  private def invokeComparison(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "<" =>
        compareNumbers(name, args, pos)(_ < _)
      case ">" =>
        compareNumbers(name, args, pos)(_ > _)
      case "=" =>
        compareNumbers(name, args, pos)(_ == _)
      case "<=" =>
        compareNumbers(name, args, pos)(_ <= _)
      case _ =>
        unknownProcedure(name, pos)

  private def invokeEqualityBuiltin(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    name match
      case "eq?" =>
        Value.BoolVal(ValueSemantics.isEq(values.head, values(1)))
      case "equal?" =>
        Value.BoolVal(ValueSemantics.isEqual(values.head, values(1)))
      case _ =>
        unknownProcedure(name, pos)

  private def invokeNumericUtility(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "abs" =>
        Value.IntVal(math.abs(requireNumber(name, requireSingleArg(name, args, pos), pos)))
      case "modulo" =>
        val (dividend, divisor) = requireBinaryNumbers(name, args, pos)
        Value.IntVal(modulo(dividend, divisor, pos))
      case "remainder" =>
        val (dividend, divisor) = requireBinaryNumbers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend % divisor)
      case "quotient" =>
        val (dividend, divisor) = requireBinaryNumbers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend / divisor)
      case "min" =>
        Value.IntVal(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos).min)
      case "max" =>
        Value.IntVal(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos).max)
      case "expt" =>
        val (base, exponent) = requireBinaryNumbers(name, args, pos)
        if exponent < 0 then throw EvalError.at(pos, s"$name expected a non-negative exponent")
        Value.IntVal(expt(base, exponent))
      case _ =>
        unknownProcedure(name, pos)

  private def invokeNumericPredicate(name: String, args: List[Value], pos: SourcePos): Value =
    val number = requireNumber(name, requireSingleArg(name, args, pos), pos)
    name match
      case "zero?" =>
        Value.BoolVal(number == 0)
      case "positive?" =>
        Value.BoolVal(number > 0)
      case "negative?" =>
        Value.BoolVal(number < 0)
      case "odd?" =>
        Value.BoolVal(number % 2 != 0)
      case "even?" =>
        Value.BoolVal(number % 2 == 0)
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
        unaryPredicate(name, args, pos) {
          case Value.IntVal(_) => true
          case _               => false
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
      case _ =>
        unknownProcedure(name, pos)

  private def modulo(dividend: Int, divisor: Int, pos: SourcePos): Int =
    if divisor == 0 then throw EvalError.at(pos, "division by zero")
    val remainder = dividend % divisor
    if remainder == 0 || Integer.signum(remainder) == Integer.signum(divisor) then remainder
    else remainder + divisor

  private def expt(base: Int, exponent: Int): Int =
    @annotation.tailrec
    def loop(factor: Int, power: Int, acc: Int): Int =
      if power == 0 then acc
      else if (power & 1) == 1 then loop(factor * factor, power >>> 1, acc * factor)
      else loop(factor * factor, power >>> 1, acc)

    loop(base, exponent, acc = 1)
