package ming

import scala.annotation.tailrec

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] object SchemeNumericBuiltins:

  val bindings: List[(String, Value)] = List(
    "+" -> Value.Builtin(
      "+",
      args => Value.IntegerValue(numericArgs("+", args).foldLeft(BigInt(0))(_ + _))
    ),
    "-" -> Value.Builtin(
      "-",
      args =>
        val numbers = numericArgs("-", args)
        requireMinArgCount("-", args, 1)
        val result =
          if numbers.length == 1 then -numbers.head
          else numbers.tail.foldLeft(numbers.head)(_ - _)
        Value.IntegerValue(result)
    ),
    "*" -> Value.Builtin(
      "*",
      args => Value.IntegerValue(numericArgs("*", args).foldLeft(BigInt(1))(_ * _))
    ),
    "/" -> Value.Builtin(
      "/",
      args =>
        val numbers = numericArgs("/", args)
        requireMinArgCount("/", args, 2)
        val result = numbers.tail.foldLeft(numbers.head) { (left, right) =>
          requireNonZeroDivisor("/", right)
          left / right
        }
        Value.IntegerValue(result)
    ),
    "<"  -> numericComparator("<")(_ < _),
    ">"  -> numericComparator(">")(_ > _),
    "="  -> numericComparator("=")(_ == _),
    "<=" -> numericComparator("<=")(_ <= _),
    "abs" -> Value.Builtin(
      "abs",
      args =>
        requireArgCount("abs", args, 1)
        Value.IntegerValue(requireInteger("abs", args.head).abs)
    ),
    "modulo" -> Value.Builtin(
      "modulo",
      args =>
        requireArgCount("modulo", args, 2)
        val dividend = requireInteger("modulo", args.head)
        val divisor  = requireInteger("modulo", args(1))
        requireNonZeroDivisor("modulo", divisor)
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
        val dividend = requireInteger("remainder", args.head)
        val divisor  = requireInteger("remainder", args(1))
        requireNonZeroDivisor("remainder", divisor)
        Value.IntegerValue(dividend % divisor)
    ),
    "quotient" -> Value.Builtin(
      "quotient",
      args =>
        requireArgCount("quotient", args, 2)
        val dividend = requireInteger("quotient", args.head)
        val divisor  = requireInteger("quotient", args(1))
        requireNonZeroDivisor("quotient", divisor)
        Value.IntegerValue(dividend / divisor)
    ),
    "min" -> Value.Builtin(
      "min",
      args =>
        requireMinArgCount("min", args, 1)
        Value.IntegerValue(numericArgs("min", args).min)
    ),
    "max" -> Value.Builtin(
      "max",
      args =>
        requireMinArgCount("max", args, 1)
        Value.IntegerValue(numericArgs("max", args).max)
    ),
    "expt" -> Value.Builtin(
      "expt",
      args =>
        requireArgCount("expt", args, 2)
        val base     = requireInteger("expt", args.head)
        val exponent = requireInteger("expt", args(1))
        if exponent.signum < 0 then throw new EvalError("expt expected a non-negative exponent")
        Value.IntegerValue(integerPower(base, exponent))
    ),
    "zero?"     -> unaryNumericPredicate("zero?")(_ == 0),
    "positive?" -> unaryNumericPredicate("positive?")(_ > 0),
    "negative?" -> unaryNumericPredicate("negative?")(_ < 0),
    "odd?"      -> unaryNumericPredicate("odd?")(_ % 2 != 0),
    "even?"     -> unaryNumericPredicate("even?")(_ % 2 == 0)
  )

  private def unaryNumericPredicate(name: String)(predicate: BigInt => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(requireInteger(name, args.head)))
    )

  private def integerPower(base: BigInt, exponent: BigInt): BigInt =
    @tailrec
    def loop(factor: BigInt, remaining: BigInt, acc: BigInt): BigInt =
      if remaining == 0 then acc
      else if (remaining % 2) == 0 then loop(factor * factor, remaining / 2, acc)
      else loop(factor * factor, remaining / 2, acc * factor)

    loop(base, exponent, BigInt(1))
