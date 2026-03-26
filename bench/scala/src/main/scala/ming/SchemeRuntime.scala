package ming

import scala.annotation.tailrec

import SchemeModel.*

private[ming] object SchemeRuntime:

  def baseEnv(): Env =
    val env = new Env(None)
    builtinBindings.foreach { case (name, value) =>
      env.define(name, value)
    }
    env

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  def render(value: Value): String =
    value match
      case Value.IntegerValue(number) => number.toString
      case Value.BooleanValue(flag)   => if flag then "#t" else "#f"
      case Value.StringValue(text)    => s""""${escapeString(text)}""""
      case Value.SymbolValue(name)    => name
      case Value.NilValue             => "()"
      case pair: Value.PairValue      => renderPair(pair)
      case Value.Builtin(name, _)     => s"#<procedure:$name>"
      case Value.Closure(Some(name), _, _, _) =>
        s"#<procedure:$name>"
      case Value.Closure(None, _, _, _) =>
        "#<procedure>"
      case Value.VoidValue =>
        "#<void>"

  def makeList(values: List[Value]): Value =
    values.foldRight(Value.NilValue: Value) { (car, cdr) =>
      Value.PairValue(car, cdr)
    }

  def ensureDistinct(names: List[String], context: String): Unit =
    if names.distinct.length != names.length then throw new EvalError(s"$context must be distinct")

  def requireArgCount(name: String, args: List[Value], exact: Int): Unit =
    if args.length != exact then throw new EvalError(s"$name expected $exact argument(s), got ${args.length}")

  private def requireMinArgCount(name: String, args: List[Value], minimum: Int): Unit =
    if args.length < minimum then
      throw new EvalError(s"$name expected at least $minimum argument(s), got ${args.length}")

  private def numericArgs(name: String, args: List[Value]): List[BigInt] =
    args.map {
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"$name expected a number, got ${render(other)}")
    }

  private def requirePair(name: String, value: Value): Value.PairValue =
    value match
      case pair @ Value.PairValue(_, _) => pair
      case other =>
        throw new EvalError(s"$name expected a pair, got ${render(other)}")

  private def properListElements(name: String, value: Value): List[Value] =
    @tailrec
    def loop(current: Value, reversedElements: List[Value]): List[Value] =
      current match
        case Value.NilValue =>
          reversedElements.reverse
        case Value.PairValue(car, cdr) =>
          loop(cdr, car :: reversedElements)
        case other =>
          throw new EvalError(s"$name expected a proper list, got ${render(other)}")

    loop(value, Nil)

  private def properListLength(name: String, value: Value): Int =
    @tailrec
    def loop(current: Value, length: Int): Int =
      current match
        case Value.NilValue =>
          length
        case Value.PairValue(_, cdr) =>
          loop(cdr, length + 1)
        case other =>
          throw new EvalError(s"$name expected a proper list, got ${render(other)}")

    loop(value, 0)

  private def renderPair(pair: Value.PairValue): String =
    @tailrec
    def loop(current: Value, reversedParts: List[String]): String =
      current match
        case Value.PairValue(car, cdr) =>
          loop(cdr, render(car) :: reversedParts)
        case Value.NilValue =>
          reversedParts.reverse.mkString("(", " ", ")")
        case other =>
          val prefix = reversedParts.reverse.mkString("(", " ", "")
          s"$prefix . ${render(other)})"

    loop(pair, Nil)

  private def escapeString(text: String): String =
    text.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  private def numericComparator(name: String)(predicate: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        val numbers = numericArgs(name, args)
        requireMinArgCount(name, args, 2)
        Value.BooleanValue(numbers.zip(numbers.tail).forall(predicate.tupled))
    )

  private def predicateBuiltin(name: String)(predicate: Value => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(args.head))
    )

  private def appendValues(args: List[Value]): Value =
    args.reverse match
      case Nil =>
        Value.NilValue
      case last :: reversedPrefixes =>
        reversedPrefixes.foldLeft(last) { (result, listValue) =>
          properListElements("append", listValue).reverse.foldLeft(result) { (cdr, item) =>
            Value.PairValue(item, cdr)
          }
        }

  private val builtinBindings: List[(String, Value)] = List(
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
          if right == 0 then throw new EvalError("division by zero")
          left / right
        }
        Value.IntegerValue(result)
    ),
    "<"  -> numericComparator("<")(_ < _),
    ">"  -> numericComparator(">")(_ > _),
    "="  -> numericComparator("=")(_ == _),
    "<=" -> numericComparator("<=")(_ <= _),
    "not" -> Value.Builtin(
      "not",
      args =>
        requireArgCount("not", args, 1)
        Value.BooleanValue(!isTruthy(args.head))
    ),
    "cons" -> Value.Builtin(
      "cons",
      args =>
        requireArgCount("cons", args, 2)
        Value.PairValue(args.head, args(1))
    ),
    "car" -> Value.Builtin(
      "car",
      args =>
        requireArgCount("car", args, 1)
        requirePair("car", args.head).car
    ),
    "cdr" -> Value.Builtin(
      "cdr",
      args =>
        requireArgCount("cdr", args, 1)
        requirePair("cdr", args.head).cdr
    ),
    "null?" -> predicateBuiltin("null?") {
      case Value.NilValue => true
      case _              => false
    },
    "list" -> Value.Builtin(
      "list",
      args => makeList(args)
    ),
    "length" -> Value.Builtin(
      "length",
      args =>
        requireArgCount("length", args, 1)
        Value.IntegerValue(BigInt(properListLength("length", args.head)))
    ),
    "append" -> Value.Builtin(
      "append",
      args => appendValues(args)
    ),
    "string?" -> predicateBuiltin("string?") {
      case Value.StringValue(_) => true
      case _                    => false
    },
    "number?" -> predicateBuiltin("number?") {
      case Value.IntegerValue(_) => true
      case _                     => false
    },
    "boolean?" -> predicateBuiltin("boolean?") {
      case Value.BooleanValue(_) => true
      case _                     => false
    },
    "pair?" -> predicateBuiltin("pair?") {
      case Value.PairValue(_, _) => true
      case _                     => false
    },
    "symbol?" -> predicateBuiltin("symbol?") {
      case Value.SymbolValue(_) => true
      case _                    => false
    }
  )
