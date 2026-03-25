package ming

import scala.annotation.tailrec

private[ming] enum Expr:

  case IntAtom(value: BigInt, pos: SourcePos)
  case BoolAtom(value: Boolean, pos: SourcePos)
  case StringAtom(value: String, pos: SourcePos)
  case Symbol(name: String, pos: SourcePos)
  case ListExpr(items: List[Expr], pos: SourcePos)

private[ming] enum Value:

  case IntVal(value: BigInt)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case SymbolVal(name: String)
  case EmptyList
  case PairVal(car: Value, cdr: Value)
  case Closure(params: List[String], body: List[Expr], env: Env)
  case Builtin(name: String, fn: (List[Value], SourcePos) => Value)
  case VoidVal

  def render: String =
    this match
      case Value.IntVal(value) =>
        value.toString

      case Value.BoolVal(value) =>
        if value then "#t" else "#f"

      case Value.StringVal(value) =>
        "\"" + Value.escapeString(value) + "\""

      case Value.SymbolVal(name) =>
        name

      case Value.EmptyList =>
        "()"

      case Value.PairVal(_, _) =>
        Value.renderPair(this)

      case Value.Closure(_, _, _) =>
        "#<procedure>"

      case Value.Builtin(name, _) =>
        s"#<procedure:$name>"

      case Value.VoidVal =>
        "#<void>"

  def typeName: String =
    this match
      case Value.IntVal(_) =>
        "number"

      case Value.BoolVal(_) =>
        "boolean"

      case Value.StringVal(_) =>
        "string"

      case Value.SymbolVal(_) =>
        "symbol"

      case Value.EmptyList =>
        "null"

      case Value.PairVal(_, _) =>
        "pair"

      case Value.Closure(_, _, _) =>
        "procedure"

      case Value.Builtin(_, _) =>
        "procedure"

      case Value.VoidVal =>
        "void"

private[ming] object Value:

  def isTruthy(value: Value): Boolean =
    value match
      case Value.BoolVal(false) =>
        false

      case _ =>
        true

  def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '\\' =>
        builder.append("\\\\")

      case '"' =>
        builder.append("\\\"")

      case '\n' =>
        builder.append("\\n")

      case '\r' =>
        builder.append("\\r")

      case '\t' =>
        builder.append("\\t")

      case ch =>
        builder.append(ch)
    }
    builder.toString

  def fromQuotedExpr(expr: Expr): Value =
    expr match
      case Expr.IntAtom(value, _) =>
        Value.IntVal(value)

      case Expr.BoolAtom(value, _) =>
        Value.BoolVal(value)

      case Expr.StringAtom(value, _) =>
        Value.StringVal(value)

      case Expr.Symbol(name, _) =>
        Value.SymbolVal(name)

      case Expr.ListExpr(items, _) =>
        Value.list(items.map(fromQuotedExpr))

  def list(items: List[Value]): Value =
    items.foldRight[Value](Value.EmptyList)(Value.PairVal(_, _))

  private def renderPair(value: Value): String =
    val builder = new StringBuilder
    builder.append('(')
    appendPairContents(value, builder)
    builder.append(')')
    builder.toString

  @tailrec
  private def appendPairContents(value: Value, builder: StringBuilder): Unit =
    value match
      case Value.PairVal(car, cdr) =>
        builder.append(car.render)
        cdr match
          case Value.EmptyList =>
            ()

          case next @ Value.PairVal(_, _) =>
            builder.append(' ')
            appendPairContents(next, builder)

          case other =>
            builder.append(" . ")
            builder.append(other.render)

      case other =>
        builder.append(other.render)

final private[ming] case class SourcePos(line: Int, col: Int):

  override def toString: String = s"$line:$col"
