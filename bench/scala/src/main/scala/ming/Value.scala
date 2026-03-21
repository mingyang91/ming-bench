package ming

import scala.annotation.tailrec

enum Value:
  case Number(value: BigInt)
  case Bool(value: Boolean)
  case Str(value: String)
  case Symbol(name: String)
  case EmptyList
  case Pair(car: Value, cdr: Value)
  case Builtin(name: String)
  case Closure(parameters: List[String], body: List[Expr], env: Env)
  case Void

  def isTruthy: Boolean = this match
    case Value.Bool(false) => false
    case _                 => true

  def render: String = this match
    case Value.Number(value) => value.toString
    case Value.Bool(value)   => if value then "#t" else "#f"
    case Value.Str(value)    => "\"" + escape(value) + "\""
    case Value.Symbol(name)  => name
    case Value.EmptyList     => "()"
    case pair @ Value.Pair(_, _) =>
      "(" + renderPair(pair) + ")"
    case Value.Builtin(_) | Value.Closure(_, _, _) =>
      "#<procedure>"
    case Value.Void =>
      "#<void>"

  private def renderPair(pair: Value): String =
    @tailrec
    def loop(current: Value, acc: List[String]): String = current match
      case Value.Pair(car, cdr) =>
        loop(cdr, car.render :: acc)
      case Value.EmptyList =>
        acc.reverse.mkString(" ")
      case other =>
        (acc.reverse :+ "." :+ other.render).mkString(" ")

    loop(pair, Nil)

  private def escape(value: String): String =
    value.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case char => char.toString
