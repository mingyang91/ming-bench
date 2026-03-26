package ming

import scala.annotation.tailrec

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeNumbers.*
import SchemeRuntime.*

private[ming] object SchemeNumericBuiltins:

  val bindings: List[(String, Value)] = List(
    "+" -> Value.Builtin(
      "+",
      args => add(numericArgs("+", args))
    ),
    "-" -> Value.Builtin(
      "-",
      args =>
        requireMinArgCount("-", args, 1)
        subtract(numericArgs("-", args))
    ),
    "*" -> Value.Builtin(
      "*",
      args => multiply(numericArgs("*", args))
    ),
    "/" -> Value.Builtin(
      "/",
      args =>
        requireMinArgCount("/", args, 2)
        divide(numericArgs("/", args))
    ),
    "<"  -> numericComparator("<")(_ < 0),
    ">"  -> numericComparator(">")(_ > 0),
    "="  -> numericComparator("=")(_ == 0),
    "<=" -> numericComparator("<=")(_ <= 0),
    "abs" -> Value.Builtin(
      "abs",
      args =>
        requireArgCount("abs", args, 1)
        SchemeNumbers.abs(requireNumber("abs", args.head))
    ),
    "modulo" -> Value.Builtin(
      "modulo",
      args =>
        requireArgCount("modulo", args, 2)
        val dividend = requireExactInteger("modulo", args.head)
        val divisor  = requireExactInteger("modulo", args(1))
        if divisor == 0 then throw new EvalError("division by zero")
        val remainder = dividend % divisor
        val result =
          if remainder == 0 || remainder.signum == divisor.signum then remainder
          else remainder + divisor
        Value.IntegerValue(result)
    ),
    "remainder" -> Value.Builtin(
      "remainder",
      args =>
        requireArgCount("remainder", args, 2)
        val dividend = requireExactInteger("remainder", args.head)
        val divisor  = requireExactInteger("remainder", args(1))
        if divisor == 0 then throw new EvalError("division by zero")
        Value.IntegerValue(dividend % divisor)
    ),
    "quotient" -> Value.Builtin(
      "quotient",
      args =>
        requireArgCount("quotient", args, 2)
        val dividend = requireExactInteger("quotient", args.head)
        val divisor  = requireExactInteger("quotient", args(1))
        if divisor == 0 then throw new EvalError("division by zero")
        Value.IntegerValue(dividend / divisor)
    ),
    "min" -> Value.Builtin(
      "min",
      args =>
        requireMinArgCount("min", args, 1)
        SchemeNumbers.min(numericArgs("min", args))
    ),
    "max" -> Value.Builtin(
      "max",
      args =>
        requireMinArgCount("max", args, 1)
        SchemeNumbers.max(numericArgs("max", args))
    ),
    "expt" -> Value.Builtin(
      "expt",
      args =>
        requireArgCount("expt", args, 2)
        val base     = requireNumber("expt", args.head)
        val exponent = requireExactInteger("expt", args(1))
        if exponent.signum < 0 then throw new EvalError("expt expected a non-negative exponent")
        integerPower(base, exponent)
    ),
    "zero?"     -> unaryNumericPredicate("zero?")(value => SchemeNumbers.compare(value, Value.IntegerValue(0)) == 0),
    "positive?" -> unaryNumericPredicate("positive?")(value => SchemeNumbers.compare(value, Value.IntegerValue(0)) > 0),
    "negative?" -> unaryNumericPredicate("negative?")(value => SchemeNumbers.compare(value, Value.IntegerValue(0)) < 0),
    "odd?" -> Value.Builtin(
      "odd?",
      args =>
        requireArgCount("odd?", args, 1)
        Value.BooleanValue(requireInteger("odd?", args.head) % 2 != 0)
    ),
    "even?" -> Value.Builtin(
      "even?",
      args =>
        requireArgCount("even?", args, 1)
        Value.BooleanValue(requireInteger("even?", args.head) % 2 == 0)
    ),
    "exact?"    -> predicateBuiltin("exact?")(SchemeNumbers.isExact),
    "inexact?"  -> predicateBuiltin("inexact?")(SchemeNumbers.isInexact),
    "integer?"  -> predicateBuiltin("integer?")(SchemeNumbers.isInteger),
    "rational?" -> predicateBuiltin("rational?")(SchemeNumbers.isRational),
    "exact->inexact" -> Value.Builtin(
      "exact->inexact",
      args =>
        requireArgCount("exact->inexact", args, 1)
        SchemeNumbers.exactToInexact(requireNumber("exact->inexact", args.head))
    ),
    "inexact->exact" -> Value.Builtin(
      "inexact->exact",
      args =>
        requireArgCount("inexact->exact", args, 1)
        SchemeNumbers.inexactToExact(requireNumber("inexact->exact", args.head))
    ),
    "numerator" -> Value.Builtin(
      "numerator",
      args =>
        requireArgCount("numerator", args, 1)
        Value.IntegerValue(SchemeNumbers.numerator(requireNumber("numerator", args.head)))
    ),
    "denominator" -> Value.Builtin(
      "denominator",
      args =>
        requireArgCount("denominator", args, 1)
        Value.IntegerValue(SchemeNumbers.denominator(requireNumber("denominator", args.head)))
    )
  )

  private def unaryNumericPredicate(name: String)(predicate: Value => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(requireNumber(name, args.head)))
    )

  private def integerPower(base: Value, exponent: BigInt): Value =
    @tailrec
    def loop(factor: Value, remaining: BigInt, acc: Value): Value =
      if remaining == 0 then acc
      else if (remaining % 2) == 0 then loop(multiply(List(factor, factor)), remaining / 2, acc)
      else loop(multiply(List(factor, factor)), remaining / 2, multiply(List(acc, factor)))

    val power = loop(base, exponent, Value.IntegerValue(1))
    if SchemeNumbers.isInexact(base) then SchemeNumbers.exactToInexact(power) else power
