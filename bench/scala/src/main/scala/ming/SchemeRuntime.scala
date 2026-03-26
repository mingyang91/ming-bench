package ming

import scala.annotation.tailrec

import SchemeModel.*
import SchemeNumbers.*

private[ming] object SchemeRuntime:

  def baseEnv(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(None)
    SchemeBuiltins.bindings(output).foreach { case (name, value) =>
      env.define(name, value)
    }
    env

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  def render(value: Value): String =
    value match
      case number @ (Value.IntegerValue(_) | Value.RationalValue(_, _) | Value.InexactValue(_)) =>
        SchemeNumbers.render(number)
      case Value.BooleanValue(flag)   => if flag then "#t" else "#f"
      case Value.StringValue(text)    => s""""${escapeString(text.text)}""""
      case Value.CharValue(codePoint) => renderChar(codePoint)
      case Value.SymbolValue(name)    => name
      case Value.NilValue             => "()"
      case pair: Value.PairValue      => renderPair(pair, render)
      case Value.RecordValue(recordType, _) =>
        s"#<record ${recordType.name}>"
      case Value.Builtin(name, _) => s"#<procedure:$name>"
      case Value.Closure(Some(name), _, _, _, _) =>
        s"#<procedure:$name>"
      case Value.Closure(None, _, _, _, _) =>
        "#<procedure>"
      case Value.VoidValue =>
        "#<void>"

  def renderForDisplay(value: Value): String =
    value match
      case Value.StringValue(text)    => text.text
      case Value.CharValue(codePoint) => codePointToString(codePoint)
      case pair: Value.PairValue      => renderPair(pair, renderForDisplay)
      case other                      => render(other)

  def makeList(values: List[Value]): Value =
    values.foldRight(Value.NilValue: Value) { (car, cdr) =>
      Value.PairValue(car, cdr)
    }

  def ensureDistinct(names: List[String], context: String): Unit =
    if names.distinct.length != names.length then throw new EvalError(s"$context must be distinct")

  def requireArgCount(name: String, args: List[Value], exact: Int): Unit =
    if args.length != exact then throw new EvalError(s"$name expected $exact argument(s), got ${args.length}")

  private def renderPair(pair: Value.PairValue, renderValue: Value => String): String =
    @tailrec
    def loop(current: Value, reversedParts: List[String]): String =
      current match
        case Value.PairValue(car, cdr) =>
          loop(cdr, renderValue(car) :: reversedParts)
        case Value.NilValue =>
          reversedParts.reverse.mkString("(", " ", ")")
        case other =>
          val prefix = reversedParts.reverse.mkString("(", " ", "")
          s"$prefix . ${renderValue(other)})"

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

  private def renderChar(codePoint: Int): String =
    codePoint match
      case 32 => "#\\space"
      case 10 => "#\\newline"
      case _  => s"#\\${codePointToString(codePoint)}"

  private def codePointToString(codePoint: Int): String =
    new String(Character.toChars(codePoint))
