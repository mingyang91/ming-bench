package ming

import scala.annotation.tailrec

enum Value:
  case Number(value: BigInt)
  case Bool(value: Boolean)
  case Str(value: StringStorage)
  case Character(value: Char)
  case Symbol(name: String)
  case EmptyList
  case Pair(car: Value, cdr: Value)
  case Builtin(name: String)
  case Closure(parameters: List[String], body: List[Expr], env: Env)
  case Void

  def isTruthy: Boolean = this match
    case Value.Bool(false) => false
    case _                 => true

  def render(state: EvalState): String = this match
    case Value.Number(value) => value.toString
    case Value.Bool(value)   => if value then "#t" else "#f"
    case Value.Str(value)    => "\"" + escape(state.readString(value)) + "\""
    case Value.Character(value) =>
      renderCharacter(value)
    case Value.Symbol(name) => name
    case Value.EmptyList    => "()"
    case pair @ Value.Pair(_, _) =>
      "(" + renderPair(pair, state) + ")"
    case Value.Builtin(_) | Value.Closure(_, _, _) =>
      "#<procedure>"
    case Value.Void =>
      "#<void>"

  def renderDisplay(state: EvalState): String = this match
    case Value.Str(value) =>
      state.readString(value)
    case Value.Character(value) =>
      value.toString
    case _ =>
      render(state)

  private def renderPair(pair: Value, state: EvalState): String =
    @tailrec
    def loop(current: Value, acc: List[String]): String = current match
      case Value.Pair(car, cdr) =>
        loop(cdr, car.render(state) :: acc)
      case Value.EmptyList =>
        acc.reverse.mkString(" ")
      case other =>
        (acc.reverse :+ "." :+ other.render(state)).mkString(" ")

    loop(pair, Nil)

  private def escape(value: String): String =
    value.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case char => char.toString

  private def renderCharacter(value: Char): String =
    value match
      case ' '  => "#\\space"
      case '\n' => "#\\newline"
      case char => s"#\\$char"

object Value:

  def immutableString(value: String): Value =
    Value.Str(StringStorage.Immutable(value))
