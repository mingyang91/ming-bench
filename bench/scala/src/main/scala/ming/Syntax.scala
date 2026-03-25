package ming

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
  case Builtin(name: String, fn: (List[Value], SourcePos) => Value)

  def render: String =
    this match
      case Value.IntVal(value) =>
        value.toString

      case Value.BoolVal(value) =>
        if value then "#t" else "#f"

      case Value.StringVal(value) =>
        "\"" + Value.escapeString(value) + "\""

      case Value.Builtin(name, _) =>
        s"#<procedure:$name>"

  def typeName: String =
    this match
      case Value.IntVal(_) =>
        "number"

      case Value.BoolVal(_) =>
        "boolean"

      case Value.StringVal(_) =>
        "string"

      case Value.Builtin(_, _) =>
        "procedure"

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

final private[ming] case class SourcePos(line: Int, col: Int):

  override def toString: String = s"$line:$col"
